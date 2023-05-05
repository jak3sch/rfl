library(tidyverse)
library(nflfastR)

var.lastSeason = 2022

# base data ----
## fantasy ----
fantasyPoints <- readRDS(url(paste0("https://github.com/jak3sch/rfl/blob/main/data/playerscores/rfl-playerscores-", var.lastSeason, ".rds?raw=true"))) %>%
  dplyr::mutate(
    points = as.numeric(points),
    week = as.integer(week)
  )

## WAR ----
franchiseWARPlayerRaw <- readr::read_csv(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-", var.lastSeason, ".csv"), col_types = "icccdd") %>% 
  dplyr::left_join(
    readr::read_csv(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-", var.lastSeason - 1, ".csv"), col_types = "icccdd") %>%
      dplyr::select(player_id, war) %>%
      dplyr::rename(war_last_season = war),
    by = "player_id"
  )

franchiseWARPlayer <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", var.lastSeason, "/export?TYPE=rosters&L=63018&APIKEY=&FRANCHISE=&W=&JSON=1"))$rosters$franchise %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(franchise_id = id) %>%
  tidyr::unnest(player) %>%
  tidyr::unnest_wider(player) %>%
  dplyr::rename(mfl_id = id) %>%
  dplyr::select(mfl_id, franchise_id) %>%
  dplyr::left_join(franchiseWARPlayerRaw %>% dplyr::select(player_id, pos, points, war, war_last_season), by = c("mfl_id" = "player_id"), multiple = "any") %>%
  dplyr::mutate(
    war_per_fpt = war / points
  ) %>%
  group_by(franchise_id) %>%
  arrange(desc(war_per_fpt)) %>%
  dplyr::ungroup()

## Official Stats ----
pbp <- nflfastR::load_pbp(var.lastSeason)
playerStatsOffense <- nflfastR::calculate_player_stats(pbp)
playerStatsDefense <- nflfastR::calculate_player_stats_def(pbp)

# awards ----
orderCols <- function(df) {
  df %>%
    utils::head(3) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(mfl_id, gsis_id, pos, points, war, rank)
}

league_awards <- franchiseWARPlayer %>%
  dplyr::select(mfl_id, pos, points, war, war_last_season) %>%
  dplyr::distinct() %>%
  dplyr::group_by(mfl_id, pos, points, war, war_last_season) %>%
  dplyr::left_join(nflreadr::load_ff_playerids() %>% dplyr::select(mfl_id, gsis_id), by = "mfl_id") %>%
  dplyr::filter(!is.na(war)) %>%
  dplyr::group_by(pos) %>%
  dplyr::arrange(desc(war)) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    nflreadr::load_players() %>% dplyr::select(display_name, last_name, gsis_id, rookie_year),
    by = "gsis_id",
    multiple = "first"
  )

## MVP ----
league_mvp <- league_awards %>%
  dplyr::arrange(desc(war)) %>%
  orderCols() %>%
  dplyr::mutate(
    award = "MVP",
    label = ifelse(rank == 1, "Most Valuable Player", paste0(rank, ". Platz"))
  )

## OPOY ----
league_opoy <- league_awards %>%
  dplyr::filter(pos %in% c("QB", "RB", "WR", "TE") & mfl_id != league_mvp$mfl_id[1]) %>%
  dplyr::arrange(desc(war)) %>%
  orderCols() %>%
  dplyr::mutate(
    award = "OPOY",
    label = ifelse(rank == 1, "Offensive Player of the Year", paste0(rank, ". Platz"))
  )

## DPOY ----
league_dpoy <- league_awards %>%
  dplyr::filter(pos %in% c("DT", "DE", "LB", "CB", "S") & mfl_id != league_mvp$mfl_id[1]) %>%
  dplyr::arrange(desc(war)) %>%
  orderCols() %>%
  dplyr::mutate(
    award = "DPOY",
    label = ifelse(rank == 1, "Defensive Player of the Year", paste0(rank, ". Platz"))
  )

## AIR POY ----
league_air <- playerStatsOffense %>%
  dplyr::filter(position == "QB") %>%
  dplyr::mutate(
    fpts_air =
      (passing_tds * 4) +
      (passing_yards * 0.04) +
      (passing_2pt_conversions * 2) +
      (interceptions * -1) +
      (sack_fumbles_lost * -1)
  ) %>%
  dplyr::select(player_id, fpts_air) %>%
  dplyr::arrange(desc(fpts_air)) %>%
  dplyr::rename(gsis_id = player_id) %>%
  dplyr::left_join(league_awards, by = "gsis_id", multiple = "first") %>%
  orderCols() %>%
  dplyr::mutate(
    award = "APOY",
    label = ifelse(rank == 1, "Air Player of the Year", paste0(rank, ". Platz"))
  )

## GROUND POY ----
league_ground <- playerStatsOffense %>%
  dplyr::mutate(
    fpts_air =
      (rushing_tds * 6) +
      (rushing_yards * 0.1) +
      (rushing_2pt_conversions * 2) +
      (rushing_fumbles_lost * -1)
  ) %>%
  dplyr::select(player_id, fpts_air) %>%
  dplyr::arrange(desc(fpts_air)) %>%
  dplyr::rename(gsis_id = player_id) %>%
  dplyr::left_join(league_awards, by = "gsis_id", multiple = "first") %>%
  orderCols() %>%
  dplyr::mutate(
    award = "GPOY",
    label = ifelse(rank == 1, "Ground Player of the Year", paste0(rank, ". Platz"))
  )

## Rookies ----
league_rookies <- league_awards %>%
  dplyr::filter(rookie_year == var.lastSeason)

### OROY ----
league_oroy <- league_rookies %>%
  dplyr::filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::arrange(desc(war)) %>%
  orderCols() %>%
  dplyr::mutate(
    award = "OROY",
    label = ifelse(rank == 1, "Offensive Rookie of the Year", paste0(rank, ". Platz"))
  )

### DROY ----
league_droy <- league_rookies %>%
  dplyr::filter(pos %in% c("DT", "DE", "LB", "CB", "S")) %>%
  dplyr::arrange(desc(war)) %>%
  orderCols() %>%
  dplyr::mutate(
    award = "DROY",
    label = ifelse(rank == 1, "Defensive Rookie of the Year", paste0(rank, ". Platz"))
  )

## Comeback
league_comeback <- nflreadr::load_rosters(var.lastSeason - 1) %>%
  dplyr::filter(status == "R/Injured") %>%
  dplyr::select(gsis_id) %>%
  dplyr::left_join(
    league_awards,
    by = "gsis_id"
  ) %>%
  dplyr::mutate(war_diff = war - war_last_season) %>%
  dplyr::arrange(desc(war_diff)) %>%
  orderCols() %>%
  dplyr::mutate(
    award = "CPOY",
    label = ifelse(rank == 1, "Comeback Player of the Year", paste0(rank, ". Platz"))
  )

combined_league_awards <- rbind(league_mvp, league_opoy, league_dpoy, league_air, league_ground, league_oroy, league_droy, league_comeback) %>%
  dplyr::left_join(
    league_awards %>%
      dplyr::select(mfl_id, dplyr::ends_with("name")),
    by = "mfl_id"
  ) %>%
  dplyr::mutate(season = var.lastSeason) %>%
  dplyr::select(season, dplyr::ends_with("id"), dplyr::ends_with("name"), pos:label)

read_data <- readr::read_csv("data/awards/rfl-player-awards.csv") %>%
  dplyr::filter(season < var.lastSeason)

readr::write_csv(rbind(read_data, combined_league_awards), "data/awards/rfl-player-awards.csv")
