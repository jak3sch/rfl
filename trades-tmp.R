library(tidyverse)
library(lubridate)

trades <- readr::read_csv("data/trades/rfl-trades.csv", col_types = "ddcdcccc")

elo <- purrr::map_df(2016:2023, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-player-elo-{x}.csv"),
    col_types = "ddccccc"
  )
})

trades_with_elo <- trades %>%
  dplyr::mutate(season = lubridate::year(date)) %>%
  dplyr::left_join(
    elo %>%
      dplyr::select(mfl_id, season, week, player_elo_post),
    by = dplyr::join_by(asset_id == mfl_id, closest(season >= season), closest(week >= week))
  ) %>%
  dplyr::mutate(player_elo_post = ifelse(is.na(player_elo_post), 1500, player_elo_post)) %>%
  dplyr::select(-timestamp, -season.x, -season.y, -week.y)


