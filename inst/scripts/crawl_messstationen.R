################################################################################
# This script starts the ETL Process for crawling messstationen                #
#                                                                              #
# Job Runs at kubernetes                                                       #
#                                                                              #
# Schedule: hourly                                                             #
#                                                                              #
# Author: Sarah Wagner                                                         #
# E-mail: sarah.wagner@inwt-statistics.de                                      #
################################################################################


# 00 Preparation ---------------------------------------------------------------
cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat("  ", names(sysinfo)[i], ":", sysinfo[i], "\n")
options(warn = 2)

sessionInfo()

# 01 Run ----------------------------------------------------------------------- 
# rm(list=ls())
library(dplyr)
library(fairqDataMessstationen)
library(fairqDbtools)

station_info <- create_station_info()
station_info <- add_download_links_last_2_months(station_info)

# for crawling historic data for a specific year use:
# station_info <- add_download_links_for_year(station_info, year = 2022)

# DOWNLOAD -----------------------------------------------------------------------------------------
station_csv_list <- station_info %>%
  filter(!is.na(url_hourly_csv)) %>%
  get_station_csv()

air_quality_measurements <- station_csv_list %>% bind_rows()

air_quality_measurements <- manage_columns(air_quality_measurements)

# WRITE TO DB --------------------------------------------------------------------------------------
# table messstationen
station_info %>%
  select(station_id, station_name, station_url) %>%
  send_data(., table = "messstationen", mode = "replace")

# table messstationen_daten
send_data(df = air_quality_measurements, table = "messstationen_daten", mode = "replace")

print("Done")
q(save = "no", status = 0)
