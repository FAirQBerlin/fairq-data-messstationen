# FUNCTION create_station_info() -------------------------------------------------------------------

# act
res <- create_station_info()

# test
test_that("create_station_info works", {
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 3)
  expect_equal(names(res), c("station_id", "station_name", "station_url"))
  expect_equal(res[res$station_name == "Wedding", "station_id"], "010")
  expect_equal(res[res$station_name == paste0("Silbersteinstra", "\u00df", "e"), "station_id"],
               "143")
  expect_equal(res[res$station_name == "Karlshorst", "station_id"], "282")
  expect_true(grepl(pattern = "mc085", res[res$station_name == "Friedrichshagen", "station_url"],
                    fixed = TRUE))
  expect_true(grepl(pattern = "mc124", res[res$station_name == "Mariendorfer Damm", "station_url"],
                    fixed = TRUE))
})

rm(list = c("res"))


# FUNCTION add_missing_karl_marx_entry() -----------------------------------------------------------

input_only_220 <- data.frame(
  station_id = c("999", "220"),
  station_name = c("Fantasy", paste0("Karl-Marx-Stra", "\u00df", "e")),
  station_url = c(
    "https://luftdaten.berlin.de/station/mc999",
    "https://luftdaten.berlin.de/station/mc220"
  )
)
input_only_221 <- data.frame(
  station_id = c("999", "220"),
  station_name = c("Fantasy", paste0("Karl-Marx-Stra", "\u00df", "e")),
  station_url = c(
    "https://luftdaten.berlin.de/station/mc999",
    "https://luftdaten.berlin.de/station/mc220"
  )
)
input_no_karl_marx <- data.frame(
  station_id = c("999"),
  station_name = c("Fantasy"),
  station_url = c("https://luftdaten.berlin.de/station/mc999")
)
input_both_karl_marx <- data.frame(
  station_id = c("220", "221"),
  station_name = c(
    paste0("Karl-Marx-Stra", "\u00df", "e"),
    paste0("Karl-Marx-Stra", "\u00df", "e")
  ),
  station_url = c(
    "https://luftdaten.berlin.de/station/mc220",
    "https://luftdaten.berlin.de/station/mc221"
  )
)

expected_result <- data.frame(
  station_id = c("220", "221", "999"),
  station_name = c(
    paste0("Karl-Marx-Stra", "\u00df", "e"),
    paste0("Karl-Marx-Stra", "\u00df", "e"),
    "Fantasy"
  ),
  station_url = c(
    "https://luftdaten.berlin.de/station/mc220",
    "https://luftdaten.berlin.de/station/mc221",
    "https://luftdaten.berlin.de/station/mc999"
  )
)

test_that("add_missing_karl_marx_entry adds entry", {
  expect_equal(
    add_missing_karl_marx_entry(input_only_220) %>% arrange(station_id),
    expected_result
  )
  expect_equal(
    add_missing_karl_marx_entry(input_only_221) %>% arrange(station_id),
    expected_result
  )
})

test_that("add_missing_karl_marx_entry throws errors", {
  expect_error(add_missing_karl_marx_entry(input_no_karl_marx))
  expect_error(add_missing_karl_marx_entry(input_both_karl_marx))
})
