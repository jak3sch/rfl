library(tidyverse)
library(lubridate)

trades <- readr::read_csv("data/trades/rfl-trades.csv", col_types = "nncncccc")

elo <- purrr::map_df(2016:2023, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-player-elo-{x}.csv")
  )
})

# create matrix for each player with weeks 1-22
player_values <- elo %>%
  dplyr::select(mfl_id) %>%
  dplyr::distinct() %>%
  dplyr::group_by(mfl_id) %>%
  tidyr::uncount(2024-2016 + 1) %>%
  dplyr::mutate(season = 2016 + row_number() - 1) %>%
  dplyr::group_by(mfl_id, season) %>%
  tidyr::uncount(22) %>% # create 22 row for each group
  dplyr::mutate(week = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    elo %>%
      dplyr::select(mfl_id, season, week, player_elo_post),
    by = c("mfl_id", "season", "week")
  ) %>%
  dplyr::mutate(player_elo_post = ifelse(season == 2016 & week == 1 & is.na(player_elo_post), 1500, player_elo_post)) %>%
  dplyr::group_by(mfl_id) %>%
  tidyr::fill(player_elo_post)

trades_with_elo

trades_with_elo <- trades %>%
  dplyr::mutate(
    season = lubridate::year(date)
  ) %>%
  dplyr::left_join(
    elo %>%
      dplyr::select(season, week) %>%
      dplyr::group_by(season) %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::distinct() %>%
      dplyr::rename(season_max_week = week),
    by = "season"
  ) %>%
  dplyr::mutate(
    sync_season = as.numeric(ifelse(week > season_max_week, season - 1, season)),
    sync_week = as.numeric(ifelse(week > season_max_week, season_max_week, week)),
  ) %>%
  dplyr::left_join(
    player_values %>%
      dplyr::select(season, week, mfl_id, player_elo_post) %>%
      dplyr::rename(player_elo = player_elo_post) %>%
      dplyr::mutate(mfl_id = as.character(mfl_id)),
    by = c("sync_season" = "season", "sync_week" = "week", "asset" = "mfl_id")
  ) %>%
  dplyr::mutate(
    player_elo = ifelse(
      season == 2016 & week == 22 & !is.na(name), 1500,
      player_elo
    )
  ) %>%
  dplyr::select(-timestamp, -date, -season_max_week, -sync_season, -sync_week)

most_tradet_players <- trades %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::group_by(asset) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::left_join(trades %>% dplyr::select(asset, name), by = "asset", multiple = "last") %>%
  dplyr::arrange(dplyr::desc(count))

most_trades_by_franchise <- trades %>%
  dplyr::select(trade_id, franchise_id) %>%
  dplyr::distinct() %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(count))

most_trades_between_teams <- trades %>%
  dplyr::select(trade_id, franchise_id) %>%
  dplyr::distinct() %>%
  dplyr::group_by(trade_id) %>%
  dplyr::mutate(
    partner_1 = lag(franchise_id),
    partner_2 = lead(franchise_id),
    partner = ifelse(is.na(partner_1), partner_2, partner_1)
  ) %>%
  dplyr::select(-partner_1, -partner_2) %>%
  group_by(franchise_id, partner) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(count))
