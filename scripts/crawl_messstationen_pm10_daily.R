rm(list=ls()) # clean workspace before execution
# 00 Preparation

add_download_links_for_year_daily <-
  function(station_info_daily, year = "2015") {
    station_info_daily %>%
      mutate(
        url_hourly_csv =
          paste0(
            .data$station_url,
            ".csv?period=24h&timespan=",
            "custom&start%5Bdate%5D=01.01.",
            year,
            # use 23:00 of last day of year so the current year is included completely
            "&start%5Bhour%5D=00&end%5Bdate%5D=31.12.",
            year,
            "&end%5Bhour%5D=00"
          )
      )
  }

# 01 Run ------------------------------------------------------------------------ 
library(dplyr)
library(fairqDataMessstationen)

station_ids <- c("042", "174")
station_info <- create_station_info() %>%
  filter(station_id %in% station_ids)

# DOWNLOAD ---------------------------------------------------------------------------------------
# The observation of daily PM10 data started on the 01.07.1998 for 042 (Neukölln) and later for 174 (Frankfurter Allee).

station_csv_list_all <- list() # Create an empty list. Data from each iteration is being added.

for (year in 1998:2023) {# Looping through 'year'. 
    print(year)
    station_info_year <- add_download_links_for_year_daily(station_info, year = year)
    
    station_csv_list <- station_info_year %>%
      filter(!is.na(url_hourly_csv)) %>%
      get_station_csv(hourly = FALSE)
    
    station_csv_list_all[[length(station_csv_list_all) + 1]] <- station_csv_list # The current station_csv_list frame is assigned to the new line . 
}

station_csv_df <- do.call(bind_rows, station_csv_list_all) 
pm10_daily <- manage_columns(station_csv_df) %>%
  select(station_id, timestamp, feinstaub_pm10) %>%
  mutate(timestamp = as.numeric(timestamp))

# CREATE FILE -----------------------------------------------------------------------------------------
write.csv(pm10_daily, file = "pm10_daily.csv", row.names = FALSE, quote = T) # You can find the .csv in data/SenUMVK/Klima
