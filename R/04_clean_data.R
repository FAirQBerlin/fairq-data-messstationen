#' Clean station csv file
#'
#' Make header, clean names and add station id as column
#'
#' @param station_csv data.frame;
#' @param id character; station id
#' @param hourly should hourly or daily data be cleaned, default: hourly
#' @return data.frame
#' @export
clean_station_csv <- function(station_csv, id, hourly = TRUE) {
  if (hourly) {
    time_format <- "%d.%m.%Y %H:%M"
  } else {
    time_format <- "%d.%m.%Y"
  }
  
  dat <- station_csv %>%
    row_to_names(row_number = 1) %>%
    filter(!.data$Messkomponente %like% "Einheit") %>%
    filter(!.data$Messkomponente %like% "^Messzeit") %>%
    rename(timestamp = .data$Messkomponente) %>%
    mutate(station_id = !!id, .before = .data$timestamp) %>%
    mutate(across(c(-.data$timestamp, -.data$station_id), as.numeric)) %>%
    mutate(timestamp = as.POSIXct(strptime(.data$timestamp,
                                           format = time_format, tz = "Europe/Berlin"))) %>%
    clean_names() 
  
  if (hourly) dat <- dat %>% add_all_hours() 
    
  return(dat)
}



#' Add all hours between min and max date
#'
#' @description Sometimes rows are completely missing when no single value is provided.
#' If rows are removed after our first import, the next import would not remove those rows
#' because it could only overwrite existing rows. So we need a row with NAs for all missing hours.
#' Since we always import complete days, we insert hours for all days (from 00:00 of the first to
#' 23:00 of the last day)
#' @param dat data.frame with columns station_id (must be unique) and timestamp
add_all_hours <- function(dat) {
  if (nrow(dat) > 0) {
    id <- unique(dat$station_id)
    stopifnot(length(id) == 1)
    min_date <- dat$timestamp %>% first_hour_of_first_period_day
    max_date <- dat$timestamp %>% last_hour_of_last_period_day
    now <- round(Sys.time(), "hour")
    if (max_date > now) max_date <- now  # min of two POSIXct does not seem to work
    dat %>%
      right_join(data.frame(timestamp = seq(min_date, max_date, by = 3600)), by = "timestamp") %>%
      mutate(station_id = id) %>%
      arrange(.data$timestamp)
  }
}


first_hour_of_first_period_day <- function(timestamp) {
  day_lt <- as.POSIXlt(min(timestamp))
  day_lt$hour <- 0
  as.POSIXct(day_lt)
}


last_hour_of_last_period_day <- function(timestamp) {
  day_lt <- as.POSIXlt(max(timestamp))
  day_lt$hour <- 23
  as.POSIXct(day_lt)
}


#' Preserve the table columns and their order.
#'
#' All db table columns that are not present in the df are added
#' and then ordered according to the db table.
#'
#' @param df data.frame;
#' @return data.frame;
#' @export
manage_columns <- function(df) {
  table_cols <- table_columns()
  df <-
    add_column(df, !!!table_cols[setdiff(names(table_cols), names(df))]) %>%
    select(
      .data$station_id,
      .data$timestamp,
      .data$feinstaub_pm10,
      .data$feinstaub_pm2_5,
      .data$stickstoffdioxid,
      .data$stickstoffmonoxid,
      .data$stickoxide,
      .data$ozon,
      .data$benzol,
      .data$toluol,
      .data$kohlenmonoxid,
      .data$schwefeldioxid
    ) %>%
    return()
}

#' messstationen_daten table columns
#'
#' @return numeric;
#' @export
table_columns <- function() {
  return(
    c(
      "feinstaub_pm10" = NA_real_,
      "feinstaub_pm2_5" = NA_real_,
      "stickstoffdioxid" = NA_real_,
      "stickstoffmonoxid" = NA_real_,
      "stickoxide" = NA_real_,
      "ozon" = NA_real_,
      "benzol" = NA_real_,
      "toluol" = NA_real_,
      "kohlenmonoxid" = NA_real_,
      "schwefeldioxid" = NA_real_
    )
  )
}
