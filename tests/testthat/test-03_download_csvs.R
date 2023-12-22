# FUNCTION read_station_csv() -------------------------------------------------------------------

# arrange
load(file.path("testdata", "station_info_df_with_links.RData"))
id <- "174"

# act
res <- read_station_csv(station_info_df_with_links, id)

# test
test_that("read_station_csv works", {
  expect_s3_class(res, "data.frame")
  expect_true(ncol(res) == 10)
})

rm(list = c("res", "station_info_df_with_links", "id"))
