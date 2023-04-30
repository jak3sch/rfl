library(ffscrapr)
library(tidyverse)
library(dplyr)
library(here)

# https://www.fantasypoints.com/nfl/articles/season/2021/fantasy-war-part-1-theory

var.lastSeason = 2016

# base data ----
starter <- purrr::map_df(var.lastSeason, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
  )
})

var.totalGames <- starter %>%
  select(season, week) %>%
  distinct() %>%
  summarise(games = n())

var.totalGames <- var.totalGames$games

## average team weekly score ----
## 1. adding the individual scores of all nine starting positions together
## 2. we'll use the average points from the top-12 (24 for RB and WR) highest-percentage started players of each week
## 3. The process is the same for flex as a single starting position, except that all players considered a starter in their native position (RB, WR, TE) are not eligible for flex, as it's presumed these players already occupy a starting spot somewhere in the league. However, the next top-12 most-started combination of RB, WR, and TE for each week do qualify.
## 4. Simply adding all nine positional averages from this process together produces the expected amount of points the average team in a 12-team half-point PPR league for any given week.

starterByWeek <- starter %>%
  filter(starter_status == "starter") %>%
  mutate(
    pos = case_when( # fasse defensive positionen zusammen
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  group_by(player_id, week, pos) %>%
  mutate(start_pct = n() / 3) %>% # berechne start % innerhalb der liga
  ungroup() %>%
  select(week, player_id, pos, start_pct, player_score) %>%
  distinct() %>%
  group_by(week, pos) %>%
  arrange(desc(start_pct), desc(player_score)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(
    eligable = case_when(
      pos %in% c("QB", "TE", "PK") & rank <= 12 ~ 1,
      pos %in% c("RB", "WR", "DL", "LB", "DB") & rank <= 24 ~ 1,
      TRUE ~ 0
    )
  ) %>%

  mutate(
    flex = case_when(
      pos %in% c("RB", "WR", "TE") & eligable == 0 ~ "FLEX", # alle offense FLEX eligable positionen werden zu flex zusammengefasst
      pos %in% c("DL", "LB", "DB") & eligable == 0 ~ "IDP", # alle defense FLEX eligable positionen werden zu idp zusammengefasst
      TRUE ~ "no"
    )
  ) %>%
  group_by(week, flex) %>%
  arrange(desc(start_pct), desc(player_score)) %>%
  mutate(rank_flex = row_number()) %>%
  ungroup() %>%
  mutate(
    eligable = case_when(
      flex == "FLEX" & rank_flex <= 24 ~ 1, # die ersten 24 flex spots bekommen eligable status
      flex == "IDP" & rank_flex <= 36 ~ 1, # die ersten 24 IDP spots bekommen eligable status
      TRUE ~ eligable
    )
  )

## avg team points & std deviation ----
avgPlayer <- starterByWeek %>%
  filter(
    eligable == 1,
    !is.na(player_score)
  ) %>%
  mutate(
    pos = ifelse(flex == "no", pos, flex)
  ) %>%
  group_by(pos) %>%
  summarise(
    points_average_player = mean(player_score),
    sd = sd(player_score)
  ) %>%
  ungroup() %>%
  mutate(
    multiplier = case_when(
      pos %in% c("QB", "TE", "PK") ~ 1,
      pos %in% c("RB", "WR", "FLEX", "DL", "LB", "DB") ~ 2,
      pos == "IDP" ~ 3
    ),
    sd = case_when(
      pos %in% c("QB", "TE", "PK") ~ sd^2,
      pos %in% c("RB", "WR", "FLEX", "DL", "LB", "DB") ~ sd^2 + sd^2,
      pos == "IDP" ~ sd^2 + sd^2 + sd^2
    )
  )

avgTeam <- avgPlayer %>%
  mutate(points_average_player = points_average_player * multiplier) %>%
  summarise(
    points = sum(points_average_player),
    sd = sqrt(sum(sd))
  )

## replacement player ----
## The idea of a replacement player is that if you have a starting player miss a game due to injury, suspension, bye, etc., you are forced to insert your next best option into your starting lineup.
## This would either be someone on your bench or from the waiver wire/free agent pool.
replacement <- starterByWeek %>%
  filter(eligable == 0) %>%
  group_by(week, pos) %>%
  arrange(desc(start_pct), desc(player_score)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(
    eligable = case_when(
      pos %in% c("QB", "TE", "PK") & rank <= 12 ~ 1,
      pos %in% c("RB", "WR", "DL", "LB", "DB") & rank <= 24 ~ 1,
      TRUE ~ 0
    )
  ) %>%

  mutate(
    flex = case_when(
      pos %in% c("RB", "WR", "TE") & eligable == 0 ~ "FLEX", # alle offense FLEX eligable positionen werden zu flex zusammengefasst
      pos %in% c("DL", "LB", "DB") & eligable == 0 ~ "IDP", # alle defense FLEX eligable positionen werden zu idp zusammengefasst
      TRUE ~ "no"
    )
  ) %>%
  group_by(week, flex) %>%
  arrange(desc(start_pct), desc(player_score)) %>%
  mutate(rank_flex = row_number()) %>%
  ungroup() %>%
  mutate(
    eligable = case_when(
      flex == "FLEX" & rank_flex <= 24 ~ 1, # die ersten 24 flex spots bekommen eligable status
      flex == "IDP" & rank_flex <= 36 ~ 1, # die ersten 24 IDP spots bekommen eligable status
      TRUE ~ eligable
    )
  ) %>%
  filter(
    eligable == 1,
    !is.na(player_score)
  ) %>%
  mutate(
    pos = ifelse(flex == "no", pos, flex)
  ) %>%
  group_by(pos) %>%
  summarise(
    points_replacement_player = mean(player_score)
  ) %>%
  ungroup() %>%
  left_join(avgPlayer %>% select(pos, points_average_player), by = "pos") %>%
  mutate(
    replacement_team_points = avgTeam$points - points_average_player + points_replacement_player,
    win_probability_replacement = pnorm(replacement_team_points, avgTeam$points, sd = avgTeam$sd),
    replacement_wins = var.totalGames * win_probability_replacement # 103 = 17 * 5 + 18 (17 für 2016-2020)
  ) %>%
  select(-points_average_player)

## WAR berechnung ----
war <- starter %>%
  select(season, week, player_id, player_name, player_score, pos) %>%
  distinct() %>%
  filter(!is.na(player_score)) %>%
  group_by(player_id, player_name, season) %>%
  summarise(
    points = sum(player_score),
    games = n(),
    .groups = "drop"
  ) %>%
  group_by(player_id, player_name) %>%
  mutate(
    points = sum(points),
    games_played = sum(games),
    games_missed = var.totalGames - games,
    .groups = "drop"
  ) %>%
  left_join(starterByWeek %>% select(player_id, pos) %>% distinct(), by = "player_id", multiple = "all") %>%
  mutate(
    pos = case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("S", "CB") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  left_join(avgPlayer %>% select(pos, points_average_player), by = "pos", multiple = "all") %>%
  mutate(
    points_per_game = points / games_played,
    points_average_player = points_average_player,
    avg_team_points = avgTeam$points,
    war_team_points = avg_team_points - points_average_player + points_per_game,
    win_probability = pnorm(war_team_points, avg_team_points, sd = avgTeam$sd), # pnorm "abritary normal distribution"; berechnet wahrscheinlichkeit aus gesuchtem wert, avg und standard deviation
  ) %>%
  filter(!is.na(win_probability)) %>%
  left_join(replacement, by = "pos", multiple = "all") %>%
  group_by(player_id, player_name) %>%
  summarise(
    points= sum(points),
    across(c(games_missed, win_probability, avg_team_points, win_probability_replacement, replacement_wins), mean),
    win_probability = ifelse(
      games_missed == 0, win_probability, (win_probability + (win_probability_replacement * games_missed)) / (games_missed + 1)
    ),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    expected_wins = var.totalGames * win_probability, #
    war = round(expected_wins - replacement_wins, 2)
  ) %>%
  left_join(starterByWeek %>% select(player_id, pos) %>% distinct(), by = "player_id", multiple = "all") %>%
  select(player_id, player_name, pos, points, war)

write.table(war, paste0("data/war/rfl-war-", var.lastSeason, ".csv"), row.names = F, col.names = T, sep = "; ")

rm(starter, var.totalGames, starterByWeek, avgPlayer, avgTeam, replacement, war)
