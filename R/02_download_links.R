#' Get urls to hourly data csv file for the last two months
#' for each station in the station info data frame and append them.
#'
#' @param station_info_df data.frame;
#' @return data.frame;
#' @export
add_download_links_last_2_months <- function(station_info_df) {
  today <- german_date_format(Sys.Date())
  about_two_months_ago <- german_date_format(Sys.Date() - 62)
  
  station_info_df %>%
    mutate(
      url_hourly_csv =
        paste0(
          .data$station_url,
          ".csv?period=1h&timespan=",
          "custom&start%5Bdate%5D=",
          about_two_months_ago,
          "&start%5Bhour%5D=00&end%5Bdate%5D=",
          today,
          # use today (time 23:00) so today is included completely
          "&end%5Bhour%5D=23"
        )
    )
}


german_date_format <- function(date) {
  format(date, "%d.%m.%Y")
}


#' Get urls to hourly data website and their csv file for the specified year
#'
#' for each station in the station info data frame and append them.
#' Builds link to csv differently because of
#' \url{https://github.com/INWT/fairq-data-messstationen/issues/9}.
#'
#' @param station_info_df data.frame;
#' @param year character;
#' @return data.frame;
#' @export
add_download_links_for_year <-
  function(station_info_df, year = "2015") {
    station_info_df %>%
      mutate(
        url_hourly_csv =
          paste0(
            .data$station_url,
            ".csv?period=1h&timespan=",
            "custom&start%5Bdate%5D=01.01.",
            year,
            # use 23:00 of last day of year so the current year is included completely
            "&start%5Bhour%5D=00&end%5Bdate%5D=31.12.",
            year,
            "&end%5Bhour%5D=23"
          )
      )
  }
