library(tidyverse)
library(lubridate)
library(tidyverse)

var_season <- 2024

find_week <- function(use_date) {
  week1_sep <- as.POSIXlt(paste0(lubridate::year(use_date), "-09-0", 1:7), tz = "GMT")
  monday1_sep <- week1_sep[week1_sep$wday == 1]
  first_game <- monday1_sep
  first_game$mday <- first_game$mday + 3
  current_week <- as.numeric(as.Date(use_date) - as.Date(first_game))%/%7 + 1

  # TODO: neue week beginnt am donnerstag - sollte aber dienstag sein

  if (current_week < 1 | current_week > 22)
    current_week <- 22
  return(current_week)
}

last_entry <- 2023090

last_entry <- readr::read_csv("data/trades/rfl-trades.csv", col_types = "nncccccc") %>%
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
    dplyr::left_join(
      jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_season, "/export?TYPE=contestPlayers&L=63018&APIKEY=aRNp3s%2BWvuWrx0WmPlrBYDoeErox&W&JSON=1"))$contest_players$player %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1),
      by = c("asset" = "id")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::as_datetime(as.numeric(timestamp)),
      week = find_week(as.Date(date))
    ) %>%
    dplyr::select(trade_id, timestamp, date, week, franchise_id, franchise, asset, name, position, team)

  readr::write_csv(trade_data, "data/trades/rfl-trades.csv", append = TRUE)
}
