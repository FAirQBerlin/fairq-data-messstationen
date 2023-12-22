#' Wrapper to read station csv files, clean them and add station id as column.
#'
#' @param station_info_df data.frame;
#' @param hourly TRUE per default files with hourly data are extracted and cleaned
#' @return list;
#' @export
get_station_csv <- function(station_info_df, hourly = TRUE) {
  lapply(station_info_df$station_id, function(id) {
    read_station_csv(station_info_df, id) %>%
      clean_station_csv(id = id, hourly = hourly) %>%
      return()
  })
}

#' Read station csv file
#'
#' @param station_info_df data.frame;
#' @param id character; station id
#' @return data.frame
#' @export
read_station_csv <- function(station_info_df, id) {
  read.csv2(station_info_df[station_info_df$station_id == id, ]$url_hourly_csv) %>%
    return()
}
