# FUNCTION add_download_links() -----------------------------------------------------

# arrange
load(file.path("testdata", "station_info_df.RData"))
expected_colnames <-
  c("station_id",
    "station_name",
    "station_url",
    "url_hourly_csv")

# act
res <- add_download_links_last_2_months(station_info_df)

# test
test_that("add_download_links_last_2_months works", {
  expect_s3_class(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true(nrow(res) == 18)
  expect_identical(expected_colnames, names(res))
  expect_true(all(grepl(".csv?", res$url_hourly_csv, fixed = TRUE)))
})

rm(list = c("res", "station_info_df"))

# FUNCTION add_download_links_for_year() -----------------------------------------------------

# arrange
load(file.path("testdata", "station_info_df.RData"))
expected_colnames <-
  c("station_id",
    "station_name",
    "station_url",
    "url_hourly_csv")

# act
res <- add_download_links_for_year(station_info_df, year = 2021)

# test
test_that("add_download_links_for_year works", {
  expect_s3_class(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true(nrow(res) == 18)
  expect_identical(expected_colnames, names(res))
  expect_true(all(grepl(".csv?", res$url_hourly_csv, fixed = TRUE)))
  expect_true(all(grepl(
    "01.01.2021", res$url_hourly_csv, fixed = TRUE
  )))
  expect_true(all(grepl(
    "31.12.2021", res$url_hourly_csv, fixed = TRUE
  )))
})

rm(list = c("res", "station_info_df"))

# FUNCTION german_date_format() --------------------------------------------------------------

test_that("german_date_format returns correct string", {
  expect_equal(german_date_format(as.Date("2022-12-11")), "11.12.2022")
})
