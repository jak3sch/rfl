# https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/

library(ffscrapr)
library(tidyverse)
library(jsonlite)

var.season <- 2022
var.week <- 6

for (var.week in 1:6) {
if(var.week == 1) {
  elo_past <- read.csv(paste0("data/elo/rfl-elo-", var.season - 1, ".csv"), colClasses=c("franchise_id" = "character", "franchise_elo_postgame" = "integer")) %>% 
    dplyr::filter(season == var.season - 1)
} else {
  elo_past <- read.csv(paste0("data/elo/rfl-elo-", var.season, ".csv"), colClasses=c("franchise_id" = "character", "franchise_elo_postgame" = "integer")) %>% 
    dplyr::filter(season == var.season)
}

elo_past <- elo_past %>% 
  dplyr::group_by(franchise_id) %>% 
  dplyr::arrange(week) %>% 
  dplyr::summarise(franchise_elo_postgame = last(franchise_elo_postgame), .groups = "drop") # der weg ist so nötig um für bye weeks in den PO daten zu kriegen

if (var.season < 2021 & var.week >= 13 | var.season >= 2021 & var.week >= 14) {
  # post season
  schedule <- readr::read_rds("data/schedule/rfl-results-postseason.rds") %>% 
    dplyr::filter(season == var.season)
} else {
  # reg season
  scores <- jsonlite::read_json(paste0("https://www48.myfantasyleague.com/", var.season, "/export?TYPE=weeklyResults&L=63018&APIKEY=&W=", var.week, "&JSON=1")) %>% 
    #purrr::pluck("weeklyResults", "franchise") %>% # pre 2020
    purrr::pluck("weeklyResults", "matchup") %>% 
    tibble::tibble() %>% 
    tidyr::unnest_wider(1) %>% 
    tidyr::unnest(1) %>% 
    tidyr::unnest_wider(1) %>% 
    dplyr::select(id, score) %>% 
    dplyr::rename(franchise_id = id) %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::distinct()
  
  schedule <- read.csv("data/rfl-schedules.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character")) %>% 
    dplyr::filter(season == var.season) %>% 
    dplyr::mutate(week = as.integer(str_remove(week, "^0+"))) %>% 
    
    # für jedes spiel zwei zeilen erstellen mit jedem team als franchise und opponent
    dplyr::mutate(
      game_id = paste0(season, week, franchise_id, opponent_id),
      away_opponent = opponent_id,
      home_opponent = franchise_id
    ) %>% 
    tidyr::gather(key, value, ends_with("_opponent")) %>% 
    dplyr::mutate(
      opponent_id = ifelse(opponent_id == value, franchise_id, opponent_id),
      franchise_id = ifelse(franchise_id == opponent_id, value, franchise_id),
    ) %>% 
    dplyr::select(game_id, season:opponent_id) %>% 
    dplyr::distinct() %>% 
    
    # punkte
    dplyr::left_join(scores %>% dplyr::rename(franchise_score = score), by = c("franchise_id")) %>% 
    dplyr::left_join(scores %>% dplyr::rename(opponent_score = score), by = c("opponent_id" = "franchise_id"))
}

elo <- schedule %>% 
  dplyr::filter(season == var.season, week == var.week) %>% 
  
  # previous elo
  dplyr::left_join(
    elo_past %>%
      dplyr::select(franchise_id, franchise_elo_postgame) %>%
      dplyr::rename(franchise_elo_pregame = franchise_elo_postgame) %>% 
      dplyr::mutate(franchise_id = as.character(franchise_id)),
    by = "franchise_id"
  ) %>% 
  dplyr::left_join(
    elo_past %>%
      dplyr::select(franchise_id, franchise_elo_postgame) %>%
      dplyr::rename(opponent_elo_pregame = franchise_elo_postgame) %>% 
      dplyr::mutate(franchise_id = as.character(franchise_id)),
    by = c("opponent_id" = "franchise_id")
  ) %>% 
  
  dplyr::mutate(
    # if week 1 of season, recalc to default value
    franchise_elo_pregame = ifelse(week == 1, round((franchise_elo_pregame * (2/3)) + (1500 * (1/3))), franchise_elo_pregame),
    opponent_elo_pregame = ifelse(week == 1, round((opponent_elo_pregame * (2/3)) + (1500 * (1/3))), opponent_elo_pregame),
    
    score_differential = round(franchise_score - opponent_score, 2),
    result = case_when(
      score_differential > 0 ~ 1, # 1 für win
      score_differential < 0 ~ 0, # 0 für loss
      T ~ 0.5 # 0.5 für tie
    ),
    forecast = 1 / (10^(-(franchise_elo_pregame - opponent_elo_pregame) / 400 ) + 1),
    k = 36,
    forecast_delta = result - forecast,
    mov_multiplier = ifelse(score_differential >= 0, log(score_differential+1) * (2.2 / franchise_elo_pregame - opponent_elo_pregame * 0.001 + 2.2), 1),
    elo_shift = round(k * forecast_delta + mov_multiplier),
  ) %>% 
  dplyr::group_by(franchise_id) %>% 
  dplyr::mutate(
    franchise_elo_postgame = first(franchise_elo_pregame) + sum(elo_shift),
    franchise_id = as.character(franchise_id)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(1:7, score_differential, franchise_elo_pregame, opponent_elo_pregame, elo_shift, franchise_elo_postgame)

if (var.week == 1) {
  write.csv(elo, paste0("data/elo/rfl-elo-", var.season, ".csv"), row.names = F)
} else {
  elo_old <- read.csv(paste0("data/elo/rfl-elo-", var.season, ".csv"), colClasses=c("franchise_id" = "character", "opponent_id" = "character"))
  write.csv(rbind(elo_old, elo), paste0("data/elo/rfl-elo-", var.season, ".csv"), row.names = F)
}
}

elo_2016 <- read.csv("data/elo/rfl-elo-2016.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))
elo_2017 <- read.csv("data/elo/rfl-elo-2017.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))
elo_2018 <- read.csv("data/elo/rfl-elo-2018.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))
elo_2019 <- read.csv("data/elo/rfl-elo-2019.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))
elo_2020 <- read.csv("data/elo/rfl-elo-2020.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))
elo_2021 <- read.csv("data/elo/rfl-elo-2021.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))
elo_2022 <- read.csv("data/elo/rfl-elo-2022.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "game_id" = "character"))

write.csv(rbind(elo_2016, elo_2017, elo_2018, elo_2019, elo_2020, elo_2021, elo_2022), "app/data/rfl-elo.csv", row.names = F)

rm(var.season, var.week, elo_old, elo, elo_past, schedule, scores, elo_2016, elo_2017, elo_2018, elo_2019, elo_2020, elo_2021, elo_2022)
