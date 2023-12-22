################################################################################
# This script should only be run once since the dataset of interest won't 
# change. After downloading, it is saved as a .csv - file. 
################################################################################

# 00 Preparation ---------------------------------------------------------------
rm(list=ls()) # clean workspace before execution
cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat("  ", names(sysinfo)[i], ":", sysinfo[i], "\n")
options(warn = 1)

sessionInfo()

# 01 Run ------------------------------------------------------------------------
# rm(list=ls())
library(dplyr)
library(fairqDataMessstationen)
library(fairqDbtools)
station_ids <- c("042", "174", "085")
station_info <- create_station_info() %>%
  filter(station_id %in% station_ids)

# DOWNLOAD -----------------------------------------------------------------------------------------
station_csv_list_all <- list() # Create an empty list. Data from each iteration is being added.

for (year in 1988:2023) {
  station_info_year <-
    add_download_links_for_year(station_info, year = year)
  
  station_csv_list <- station_info_year %>%
    filter(!is.na(url_hourly_csv)) %>%
    get_station_csv()
  
  station_csv_list_all[[length(station_csv_list_all) + 1]] <-
    station_csv_list # The current station_csv_list frame is assigned to the new line .
}


station_csv_list_all <-
  do.call(bind_rows, station_csv_list_all) # The dataframes in the list object are put together.

air_quality_measurements <- manage_columns(station_csv_list_all) %>%
  filter(timestamp > as.POSIXct("1988-02-03 00:00:00", tz = "Europe/Berlin")) %>%
  mutate(timestamp = as.numeric(timestamp)) %>%
  select(station_id, timestamp, stickstoffdioxid, ozon) %>%
  arrange(desc(station_id))

# We observe two extraordinary spikes for NO2 Frankfurter Allee. Therefore, we uniformly
# cut back every numeric value exceeding 250 to 250. Values above 1000 were transformed to NA.

columns_to_process <- c("stickstoffdioxid", "ozon")

for (column in columns_to_process) {
  air_quality_measurements[, column] <-
    ifelse(
      air_quality_measurements[, column] > 1000,
      NA,
      ifelse(
        air_quality_measurements[, column] > 250,
        250,
        air_quality_measurements[, column]
      )
    )
}

# Separate pollutants: NO2 (42, 174) and  Ozon (85).
no2_42_174 <-
  air_quality_measurements[air_quality_measurements$station_id %in% c("042", "174"), c("timestamp", "station_id", "stickstoffdioxid")]
ozon_85 <-
  air_quality_measurements[air_quality_measurements$station_id == "085", c("timestamp", "station_id", "ozon")]

# CREATE FILE -----------------------------------------------------------------------------------------
write.csv( 
  no2_42_174,
  file = "no2_42_174.csv",
  row.names = FALSE,
  quote = T
) # You can find the .csv in data/SenUMVK/Klima

write.csv(
  ozon_85,
  file = "ozon_85.csv",
  row.names = FALSE,
  quote = T
) # You can find the .csv in data/SenUMVK/Klima
