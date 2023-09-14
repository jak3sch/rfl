library(tidyverse)
library(nflverse)

f_create_def_pct <- function(df) {
  df %>% 
    
}

rules <- jsonlite::read_json("https://www45.myfantasyleague.com/2023/export?TYPE=rules&L=63018&APIKEY=aRNp3s%2BWvuWsx12mPlrBYDoeErox&JSON=1")$rules$positionRules %>% 
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest(rule) %>% 
  tidyr::unnest_wider(rule) %>% 
  tidyr::unnest(event) %>% 
  tidyr::unnest(points) %>% 
  tidyr::unnest(range) %>% 
  dplyr::mutate(
    points = as.numeric(stringr::str_remove(points, "[*]")),
    range = stringr::str_split(range, "(?<!^)-"),
    range_min = purrr::map_chr(range, 1),
    range_max = purrr::map_chr(range, 2),
    position = stringr::str_split(positions, "\\|")
  ) %>% 
  dplyr::select(-range, -positions) %>% 
  tidyr::unnest(position)

offense_stats <- nflreadr::load_ff_opportunity(stat_type = "weekly") %>% 
  # rename stats to mfl rules: https://api.myfantasyleague.com/2023/export?TYPE=allRules&JSON=1
  dplyr::mutate(
    FL = rec_fumble_lost + rush_fumble_lost
  ) %>% 
  dplyr::rename(
    "#P" = pass_touchdown_exp,
    PY = pass_yards_gained_exp,
    P2 = pass_two_point_conv_exp,
    "#R" = rush_touchdown_exp,
    RY = rush_yards_gained_exp,
    R2 = rush_two_point_conv_exp,
    "#C" = rec_touchdown_diff,
    CY = rec_yards_gained_exp,
    C2 = rec_two_point_conv_exp,
    #FC fumble recoveries nicht in stats enthalten
    IN = pass_interception_exp,
    CC = receptions_exp,
    # "#FR" offense fumble recovery tds nicht in stats enthalten
  ) %>% 
  tidyr::gather(
    event, value, c("FL", "#P", "PY", "P2", "#R", "RY", "R2", "#C", "CY", "C2", "IN", "CC")
  ) %>% 
  dplyr::select(season, week, player_id, full_name, position, event, value) %>% 
  dplyr::filter(!is.na(player_id)) %>% 
  dplyr::left_join(
    rules %>% 
      dplyr::mutate(event = as.character(event)),
    by = c("event", "position")
  ) %>% 
  dplyr::mutate(
    xfpts_event = ifelse(value >= range_min & value <= range_max, value * points, 0)
  ) %>% 
  dplyr::group_by(
    season, week, player_id, full_name
  ) %>% 
  dplyr::summarise(xfpts = round(sum(xfpts_event), 2), .groups = "drop")
  
defense_benchmarks <- nflreadr::load_player_stats(seasons = 2012:nflreadr::most_recent_season(), stat_type = "defense") %>% 
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>% 
      dplyr::select(gsis_id, pfr_id),
    by = c("player_id" = "gsis_id")
  ) %>% 
  dplyr::filter(!is.na(pfr_id)) %>% 
  dplyr::left_join(
    nflreadr::load_snap_counts(season = 2012:nflreadr::most_recent_season()) %>% 
      dplyr::select(season, week, pfr_player_id, defense_snaps, defense_pct),
    by = c("season", "week", "pfr_id" = "pfr_player_id")
  ) %>% 
  dplyr::group_by(position_group) %>% 
  dplyr::mutate(pos_avg_defense_pct = mean(defense_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(defense_pct >= pos_avg_defense_pct, defense_snaps > 0) %>% 
  dplyr::select(-pos_avg_defense_pct) %>% 
  dplyr::mutate(
    avg_tackle_solo_pct = def_tackles_solo / defense_snaps,
    avg_tackle_assist_pct = def_tackles_with_assist / defense_snaps,
    avg_tackles_for_loss_pct = def_tackles_for_loss / defense_snaps,
    avg_interceptions_pct = def_interceptions / defense_snaps,
    avg_pass_defended_pct = def_pass_defended / defense_snaps,
    avg_sacks_pct = def_sacks / defense_snaps,
    avg_fumbles_forced_pct = def_fumbles_forced / defense_snaps,
  ) %>% 
  dplyr::group_by(position_group) %>% 
  dplyr::summarise(
    dplyr::across(dplyr::ends_with("_pct"), \(x) mean(x, na.rm = TRUE))
  )

defense_stats <- nflreadr::load_player_stats(stat_type = "defense") %>%
  dplyr::select(season, week, player_id, player_name, position, position_group) %>% 
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>% 
      dplyr::select(gsis_id, pfr_id),
    by = c("player_id" = "gsis_id")
  ) %>% 
  dplyr::left_join(
    nflreadr::load_snap_counts() %>% 
      dplyr::select(season, week, pfr_player_id, defense_snaps),
    by = c("season", "week", "pfr_id" = "pfr_player_id")
  ) %>%
  dplyr::left_join(
    defense_benchmarks,
    by = "position_group"
  ) %>% 
  
  # exp stats erstellen
  dplyr::mutate(
    TKD = defense_snaps * avg_tackle_solo_pct,
    ASD = defense_snaps * avg_tackle_assist_pct,
    IC = defense_snaps * avg_interceptions_pct,
    PD = defense_snaps * avg_pass_defended_pct,
    SK = defense_snaps * avg_sacks_pct,
    FF = defense_snaps * avg_fumbles_forced_pct
  ) %>% 
  dplyr::select(-dplyr::ends_with("_pct")) %>% 
  tidyr::gather(
    event, value, c("TKD", "ASD", "IC", "PD", "SK", "FF")
  ) %>% 
  dplyr::left_join(
    rules %>% 
      dplyr::mutate(event = as.character(event)),
    by = c("event", "position_group" = "position")
  ) %>% 
  dplyr::mutate(
    xfpts_event = ifelse(value >= range_min & value <= range_max, value * points, 0)
  ) %>% 
  dplyr::group_by(
    season, week, player_id, player_name
  ) %>% 
  dplyr::summarise(xfpts = round(sum(xfpts_event), 2), .groups = "drop")
  
FG FG Made
MG FG Missed
EP XP Made
EM XP Missed
"#UT" punt return TD
UY Punt return Yds
"#KT" Kick return TD
KY kick return yds
"#BF" blocked fg td
BLF blocked fg
"#MF" missed fg td
"#BP" blocked punt return td
BLP blocked punt
BLE blocked XP

"#DR" defense fumble recovery td
"#IR" interception return td
SF safeties