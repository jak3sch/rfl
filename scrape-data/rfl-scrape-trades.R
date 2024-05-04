library(tidyverse)
library(lubridate)

var_season <- 2024

last_entry <- readr::read_csv("data/trades/rfl-trades.csv") %>%
  dplyr::select(trade_id) %>%
  dplyr::filter(trade_id == max(trade_id)) %>%
  dplyr::distinct() %>%
  dplyr::pull()

trade_data_raw <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_season, "/export?TYPE=transactions&L=63018&TRANS_TYPE=TRADE&JSON=1"))$transactions$transaction %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::select(timestamp, franchise, franchise2, franchise1_gave_up, franchise2_gave_up) %>%
  dplyr::arrange(timestamp) %>%
  dplyr::mutate(
    trade_id = paste0(var_season, sprintf("%003d", row_number()))
  ) %>%
  dplyr::filter(trade_id > last_entry)

if (dim(trade_data_raw)[1] != 0) {
  franchise1 <- trade_data_raw %>%
    dplyr::select(trade_id, franchise, franchise1_gave_up) %>%
    tidyr::separate_rows(franchise1_gave_up, sep = ",") %>%
    dplyr::filter(franchise1_gave_up != "") %>%
    dplyr::rename(asset = franchise1_gave_up, franchise_id = franchise) %>%
    dplyr::mutate(franchise = "franchise_1")

  franchise2 <- trade_data_raw %>%
    dplyr::select(trade_id, franchise2, franchise2_gave_up) %>%
    tidyr::separate_rows(franchise2_gave_up, sep = ",") %>%
    dplyr::filter(franchise2_gave_up != "") %>%
    dplyr::rename(asset = franchise2_gave_up, franchise_id = franchise2) %>%
    dplyr::mutate(franchise = "franchise_2")

  trade_data <- rbind(franchise1, franchise2) %>%
    dplyr::arrange(trade_id) %>%
    dplyr::left_join(
      trade_data_raw %>%
        dplyr::select(trade_id, timestamp),
      by = "trade_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(date = as_datetime(as.numeric(timestamp))) %>%
    dplyr::left_join(
      jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_season, "/export?TYPE=contestPlayers&L=63018&APIKEY=aRNp3s%2BWvuWrx0WmPlrBYDoeErox&W&JSON=1"))$contest_players$player %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1),
      by = c("asset" = "id")
    ) %>%
    dplyr::select(trade_id, timestamp, date, franchise_id, franchise, asset, name, position, team)

  readr::write_csv(trade_data, "data/trades/rfl-trades.csv", append = TRUE)
}
