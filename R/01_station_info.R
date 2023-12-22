#' Wrapper to create mapping table with station id, name & url.
#'
#' @param url character;
#' @return data.frame
#' @export
create_station_info <-
  function(url = "https://luftdaten.berlin.de") {
    station_table <- get_station_table(url)
    station_info <-
      data.frame(
        station_id = unlist(lapply(station_table, get_station_id)),
        station_name = unlist(lapply(station_table, get_station_name)),
        station_url = unlist(lapply(station_table, get_station_url, url))
      )
    station_info <- add_missing_karl_marx_entry(station_info)
    return(station_info)
  }

#' Crawls station table and returns html elements.
#'
#' Requests the body of the station table and returns the html
#' elements within, which are all the table rows (<tr>).
#'
#' @param url character;
#' @return xml_nodeset;
#' @importFrom dplyr %>%
#' @export
get_station_table <- function(url) {
  read_html(paste0(url, "/lqi")) %>%
    html_elements("tbody") %>%
    html_children() %>%
    return()
}


#' Get each station's url
#'
#' Go into button element of the table row (<tr>) and get the relative path to station url,
#' then paste it with the base url.
#'
#' @param table_row html_node; html table row with station info
#' @param url character;
#' @return character;
#' @export
get_station_url <- function(table_row, url) {
  stations <- table_row %>%
    html_element(".lmn-button") %>%
    html_attr("href")
  station_abs_path <- paste0(url, stations)
  return(station_abs_path)
}

#' Get each station's name
#'
#' Get the text element of the table row (<tr>) and get the station name.
#'
#' @param table_row html_node; html table row with station info
#' @return character;
#' @importFrom dplyr %>%
#' @export
get_station_name <- function(table_row) {
  table_row %>%
    html_text2() %>%
    gsub(pattern = "\\n(.*)", replacement = "") %>%
    gsub(pattern = "\\d", replacement = "") %>%
    trimws(which = "both") %>%
    return()
}

#' Get each station's id
#'
#' Get the text element of the table row (<tr>) and get the station id
#'
#' @param table_row html_node; html table row with station info
#' @return character;
#' @importFrom dplyr %>%
#' @export
get_station_id <- function(table_row) {
  table_row %>%
    html_text2() %>%
    gsub(pattern = "\\n(.*)", replacement = "") %>%
    gsub(pattern = "\\D", replacement = "") %>%
    return()
}



#' Add missing entry for Karl-Marx-Str.
#'
#' The Karl-Marx-Str. station (ID 220) is being replaced temporarily due to construction works.
#' The replacement station has the ID 221.
#' No matter which date is crawled, we want both IDs to be in the list of crawled stations.
#' Source: https://luftdaten.berlin.de/station/mc221#station-info
#' 
#' @param station_info data frame with columns station_name, station_id and station_url, containing
#' all stations crawled from the website
#' 
#' @return data frame with additional row. If the original data frame contained ID 220, we add ID
#' 221 and vice versa.
add_missing_karl_marx_entry <- function(station_info) {
  karl_marx_row <- station_info %>%
    filter(.data$station_name == paste0("Karl-Marx-Stra", "\u00df", "e"))
  
  stopifnot(nrow(karl_marx_row) == 1)
  
  additional_row <- karl_marx_row %>%
    mutate(
      station_url = ifelse(
        .data$station_id == "220",
        gsub("220", "221", .data$station_url),
        gsub("221", "220", .data$station_url)
      ),
      station_id = ifelse(.data$station_id == "220", "221", "220"),
    )
  
  bind_rows(station_info, additional_row)
}
