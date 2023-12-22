load(file.path("testdata", "station_csv_174.RData"))
id <- "174"

# FUNCTION clean_station_csv() ---------------------------------------------------------------------

# arrange
expected_colnames <-
  c(
    "station_id",
    "timestamp",
    "feinstaub_pm10",
    "feinstaub_pm2_5",
    "stickstoffdioxid",
    "stickstoffmonoxid",
    "stickoxide",
    "ozon",
    "benzol",
    "toluol",
    "kohlenmonoxid"
  )

# act
res <- clean_station_csv(station_csv_174, id)

# test
test_that("clean_station_csv works", {
  expect_s3_class(res, "data.frame")
  expect_true(ncol(res) == 11)
  expect_true(nrow(res) == 48)
  expect_identical(expected_colnames, names(res))
  expect_true(all(res$station_id == "174"))
  expect_s3_class(res$timestamp, "POSIXct")
})

rm(list = c("res"))



# FUNCTION add_all_hours() -------------------------------------------------------------------------
# arrange

input_df <- data.frame(
  station_id = rep("010", 4),
  timestamp = as.POSIXct(
    c(
      "2022-05-20 03:00:00",
      "2022-05-20 04:00:00",
      "2022-05-20 06:00:00",
      "2022-05-20 09:00:00"
    ),
    tz = "CET"
  ),
  feinstaub_pm2_5 = c(14, NA, 1, 57),
  feinstaub_pm10 = rep(NA, 4)
)
expected <- data.frame(
  station_id = rep("010", 24),
  timestamp = as.POSIXct(
    c(
      "2022-05-20 00:00:00",
      "2022-05-20 01:00:00",
      "2022-05-20 02:00:00",
      "2022-05-20 03:00:00",
      "2022-05-20 04:00:00",
      "2022-05-20 05:00:00",
      "2022-05-20 06:00:00",
      "2022-05-20 07:00:00",
      "2022-05-20 08:00:00",
      "2022-05-20 09:00:00",
      "2022-05-20 10:00:00",
      "2022-05-20 11:00:00",
      "2022-05-20 12:00:00",
      "2022-05-20 13:00:00",
      "2022-05-20 14:00:00",
      "2022-05-20 15:00:00",
      "2022-05-20 16:00:00",
      "2022-05-20 17:00:00",
      "2022-05-20 18:00:00",
      "2022-05-20 19:00:00",
      "2022-05-20 20:00:00",
      "2022-05-20 21:00:00",
      "2022-05-20 22:00:00",
      "2022-05-20 23:00:00"
    ),
    tz = "CET"
  ),
  feinstaub_pm2_5 = c(NA, NA, NA, 14, NA, NA, 1, NA, NA, 57, rep(NA, 14)),
  feinstaub_pm10 = rep(NA, 24)
)

# act
res <- add_all_hours(input_df)

# assert
test_that("add_all_hours fills gaps", {
  expect_equal(res, expected)
})


# arrange
input_df <- data.frame(station_id = c("010", "020"))

# assert
test_that("add_all_hours throws error if ID is not unique", {
  expect_error(add_all_hours(input_df), "length\\(id\\) == 1")
})


# arrange
input_df <-
  data.frame(station_id = c(1, 1), timestamp = as.POSIXct(c("2022-01-01", "2099-01-01")))

# act
res <- add_all_hours(input_df)
max_timestamp_in_res <- max(res$timestamp)

# assert
test_that("add_all_hours does not add hours after now", {
  expect_true(max_timestamp_in_res < as.POSIXct("2099-01-01"))
})


# FUNCTION manage_columns() ------------------------------------------------------------------------
# arrange
station_csv_174 <- clean_station_csv(station_csv_174, id)

# act
res <- manage_columns(station_csv_174)

# test
test_that("manage_columns works", {
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) == 48)
  expect_true(ncol(res) == 12)
  expect_true(all(names(table_columns()) %in% colnames(res)))
})

# FUNCTION table_columns() -------------------------------------------------------------------------
# arrange
expected_names <-
  c(
    "feinstaub_pm10",
    "feinstaub_pm2_5",
    "stickstoffdioxid",
    "stickstoffmonoxid",
    "stickoxide",
    "ozon",
    "benzol",
    "toluol",
    "kohlenmonoxid",
    "schwefeldioxid"
  )
# act
res <- table_columns()

# test
test_that("table_columns works", {
  expect_type(res, "double")
  expect_length(res, 10)
  expect_identical(names(res), expected_names)
})
