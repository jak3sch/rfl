# https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/

library(ffscrapr)
library(tidyverse)
library(jsonlite)

var.season <- 2022
var.week <- 1

for (var.week in 2:3) {
elo_past <- read.csv("data/elo/rfl-elo-2022.csv", colClasses=c("franchise_id" = "character", "franchise_elo_postgame" = "integer")) %>% 
  filter(season == var.season & week == var.week - 1) %>% 
  select(franchise_id, franchise_elo_postgame) %>% 
  distinct()

schedule <- read.csv("data/rfl-schedules.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character")) %>% 
  dplyr::filter(season == var.season)

scores <- jsonlite::read_json(paste0("https://www48.myfantasyleague.com/", var.season, "/export?TYPE=weeklyResults&L=63018&APIKEY=&W=", var.week, "&JSON=1")) %>% 
  #purrr::pluck("weeklyResults", "franchise") %>% 
  purrr::pluck("weeklyResults", "matchup") %>% 
  tibble::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest(1) %>% 
  tidyr::unnest_wider(1) %>% 
  select(id, score) %>% 
  rename(franchise_id = id) %>% 
  mutate(score = as.numeric(score)) %>% 
  distinct()

elo <- schedule %>% 
  mutate(week = as.integer(str_remove(week, "^0+"))) %>% 
  filter(season == var.season, week == var.week) %>% 
  
  # für jedes spiel zwei zeilen erstellen mit jedem team als franchise und opponent
  mutate(
    game_id = paste0(season, week, franchise_id, opponent_id),
    away_opponent = opponent_id,
    home_opponent = franchise_id
  ) %>% 
  gather(key, value, ends_with("_opponent")) %>% 
  mutate(
    opponent_id = ifelse(opponent_id == value, franchise_id, opponent_id),
    franchise_id = ifelse(franchise_id == opponent_id, value, franchise_id),
  ) %>% 
  select(game_id, season:opponent_id) %>% 
  distinct() %>% 
  
  # punkte
  left_join(scores %>% rename(franchise_score = score), by = c("franchise_id")) %>% 
  left_join(scores %>% rename(opponent_score = score), by = c("opponent_id" = "franchise_id")) %>% 
  
  # previous elo
  left_join(
    elo_past %>%
      select(franchise_id, franchise_elo_postgame) %>%
      rename(franchise_elo_pregame = franchise_elo_postgame) %>% 
      mutate(franchise_id = as.character(franchise_id)),
    by = "franchise_id"
  ) %>% 
  left_join(
    elo_past %>%
      select(franchise_id, franchise_elo_postgame) %>%
      rename(opponent_elo_pregame = franchise_elo_postgame) %>% 
      mutate(franchise_id = as.character(franchise_id)),
    by = c("opponent_id" = "franchise_id")
  ) %>% 
  
  mutate(
    # if week 1 of season, uncomment these lines
    #franchise_elo_pregame = round((franchise_elo_pregame * (2/3)) + (1500 * (1/3))),
    #opponent_elo_pregame = round((opponent_elo_pregame * (2/3)) + (1500 * (1/3))),
    
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
  group_by(franchise_id) %>% 
  mutate(
    franchise_elo_postgame = first(franchise_elo_pregame) + sum(elo_shift),
    franchise_id = as.character(franchise_id)
  ) %>% 
  ungroup() %>% 
  select(1:7, score_differential, franchise_elo_pregame, opponent_elo_pregame, elo_shift, franchise_elo_postgame)

elo_old <- read.csv("data/elo/rfl-elo-2022.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character"))
write.csv(rbind(elo_old, elo), "data/elo/rfl-elo-2022.csv", row.names = F)
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
