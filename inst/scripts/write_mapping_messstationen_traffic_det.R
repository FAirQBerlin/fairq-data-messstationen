# This scripts writes the mapping between measuring stations and traffic detectors to the database.
# For a query to check the mapping using human-readable names, see
# https://github.com/INWT/fairq-data-messstationen/issues/25

library(fairqDataMessstationen)

mapping <-
  data.frame(
    station_id = rep(c(143, 124, 117, 174, 190, 220), each = 2) %>% as.character,
    mq_name = c(
      "TE384",
      "TE385",
      "TE014",
      "TE530",
      "TE386",
      "TE395",
      "TE393",
      "TE394",
      "TE561",
      "TE560",
      "TE528",
      "TE529"
    )
  )

print(mapping)

send_data(mapping, "mapping_messstationen_traffic_det", mode = "replace")
