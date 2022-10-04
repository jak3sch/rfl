library(tidyverse)
library(nflreadr)

var.season = nflreadr::get_current_season()
var.week = nflreadr::get_current_week()

starter <- read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-2022.csv", colClasses = c("franchise_id" = "character"))
schedule <- read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/rfl-schedules.csv", colClasses = c("franchise_id" = "character", "opponent_id" = "character")) %>% 
  dplyr::filter(season == var.season) %>% 
  dplyr::select(-season) %>% 
  dplyr::mutate(
    game_id = paste0(week, franchise_id, opponent_id)
  )

points <- starter %>% 
  dplyr::group_by(week, franchise_id) %>% 
  dplyr::summarise(
    pf = sum(player_score[starter_status == "starter"], na.rm = T),
    pp = sum(player_score[should_start == 1], na.rm = T),
    coach = pf - pp,
    .groups = "drop"
  )

results <- points %>% 
  dplyr::left_join(
    schedule %>% 
      tidyr::gather(key, franchise_id, c(franchise_id, opponent_id)) %>% 
      dplyr::select(week, franchise_id, game_id),
    by = c("franchise_id", "week")
  ) %>% 
  dplyr::left_join(
    schedule %>% 
      dplyr::rename(away_id = franchise_id, home_id= opponent_id) %>% 
      dplyr::select(-week),
    by = "game_id"
  ) %>% 
  dplyr::mutate(opponent_id = ifelse(franchise_id == away_id, home_id, away_id)) %>% 
  dplyr::left_join(
    points %>% 
      dplyr::select(week, franchise_id, pf_opponent = pf),
    by = c("week", "opponent_id" = "franchise_id")
  ) %>% 
  dplyr::mutate(
    win = ifelse(pf - pf_opponent > 0, 1, 0)
  ) %>%
  dplyr::left_join(
    points %>% 
      dplyr::select(week, franchise_id, pf) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by (week) %>% 
      dplyr::arrange(pf) %>% 
      dplyr::mutate(all_play_wins = row_number() - 1) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-pf),
    by = c("week", "franchise_id")
  ) %>% 
  dplyr::select(-home_id, -away_id, -game_id, -opponent_id, -pf_opponent)

true_standing <- purrr::map_df(1:var.week, function(x) {
  results %>% 
    dplyr::filter(week <= x) %>% 
    dplyr::group_by(franchise_id) %>% 
    dplyr::summarise(across(c(win, pf, pp, coach, all_play_wins), sum), .groups = "drop") %>% 
  
    dplyr::arrange(pf) %>% 
    dplyr::mutate(pf_rank = row_number()) %>%
    
    dplyr::arrange(pp) %>% 
    dplyr::mutate(pp_rank = row_number()) %>% 
    
    dplyr::arrange(win) %>% 
    dplyr::mutate(record_rank = row_number()) %>% 
    
    dplyr::arrange(all_play_wins) %>% 
    dplyr::mutate(all_play_rank = row_number()) %>% 
    
    dplyr::arrange(coach) %>% 
    dplyr::mutate(
      coach_rank = row_number(),
      week = x,
      true_standing = pf_rank + pp_rank + record_rank + all_play_rank + coach_rank
    ) %>% 
    dplyr::group_by(week) %>% 
    dplyr::arrange(desc(true_standing)) %>% 
    dplyr::mutate(true_rank = row_number())
})

write.csv(true_standing, "data/rfl-true-standing.csv", row.names = F)

rm(var.season, var.week, starter, schedule, points, results, true_standing)
