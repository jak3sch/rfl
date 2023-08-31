# calc weekly totals from starter ----
results_weekly <- starter %>%
  # by position
  dplyr::group_by(franchise_id, season, week, starter_status, should_start, pos) %>%
  dplyr::summarise(
    pos_score = sum(player_score, na.rm = T),
    .groups = "drop"
  ) %>%
  tidyr::spread(starter_status, pos_score) %>%

  # bugfix bei 100% eff, NA values werden zu 0
  dplyr::mutate(
    nonstarter = ifelse(is.na(nonstarter), 0, nonstarter),
    starter = ifelse(is.na(starter), 0, starter)
  ) %>%

  # weekly totals
  dplyr::group_by(franchise_id, season, week) %>%
  dplyr::mutate(
    points_potential = ifelse(
      should_start == 1, (starter + nonstarter), 0
    )
  ) %>%
  dplyr::group_by(franchise_id, season, week, pos) %>%
  dplyr::summarise(
    starter = sum(starter),
    nonstarter = sum(nonstarter),
    potential = sum(points_potential),
    .groups = "drop"
  ) %>%

  dplyr::mutate(
    unit = ifelse(
      pos %in% c("QB", "RB", "WR", "TE", "PK"), "offense", "defense"
    )
  ) %>%

  tidyr::gather(
    key, score, c(starter, nonstarter, potential)
  ) %>%
  dplyr::mutate(
    key = paste(unit, pos, key, sep = "_")
  ) %>%
  dplyr::select(-pos, -unit) %>%
  tidyr::spread(key, score) %>%
  replace(is.na(.), 0) %>%

  dplyr::group_by(franchise_id, season, week) %>%
  dplyr::mutate(
    points_for = rowSums(across(ends_with("_starter"), sum)),
    points_bank = rowSums(across(ends_with("nonstarter"), sum)),
    points_potential = rowSums(across(ends_with("potential"), sum)),
    efficiency = points_for / points_potential,
    offense_starter = rowSums(across(starts_with("offense") & ends_with("_starter"))),
    defense_starter = rowSums(across(starts_with("defense") & ends_with("_starter"))),
    offense_nonstarter = rowSums(across(starts_with("offense") & ends_with("nonstarter"))),
    defense_nonstarter = rowSums(across(starts_with("defense") & ends_with("nonstarter"))),
    offense_potential = rowSums(across(starts_with("offense") & ends_with("potential"))),
    defense_potential = rowSums(across(starts_with("defense") & ends_with("potential")))
  ) %>%

  dplyr::select(
    season, week, franchise_id,
    starts_with("points"),
    efficiency,
    ends_with("_starter"),
    ends_with("nontarter"),
    ends_with("potential")
  ) %>%
  dplyr::mutate(
    season_type = case_when(
      season == 2016 & week <= 13 ~ "REG",
      season == 2016 & week >= 18 ~ "NONE",
      season >= 2017 & season <= 2020 & week <= 12 ~ "REG",
      season >= 2017 & season <= 2020 & week >= 17 ~ "NONE",
      season >= 2021 & week <= 13 ~ "REG",
      season >= 2021 & week >= 18 ~ "NONE",
      TRUE ~ "POST"
    )
  ) %>%
  dplyr::filter(season_type != "NONE")

# calc season stats from weekly results ----
results_season <- results_weekly %>%
  dplyr::filter(
    season_type == "REG"
  ) %>%
  dplyr::group_by(franchise_id, season) %>%
  dplyr::summarise(
    across(where(is.numeric), sum),
    efficiency = points_for / points_potential,
    .groups = "drop"
  )

# calc regular season stats ----
regularseason <- results_weekly %>%
  dplyr::mutate(
    week = sprintf("%02d", week)
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::arrange(points_for) %>%
  dplyr::mutate(
    all_wins_week = row_number() - 1
  ) %>%
  dplyr::ungroup()

# schedule ----
rfl_schedules <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/rfl-schedules.csv", col_types = "dccc")

# durch double header matchups mit unterschiedlichen home/away schedules muss erst einmal der spielplan normalisiert werden
rfl_schedule_home_games <- latest_franchises %>%
  dplyr::select(franchise_id) %>% # hole alle team IDs aus latest franchises
  dplyr::left_join(rfl_schedules, by = "franchise_id", multiple = "all") # standard schedule daten, wo das franchise home team ist, werden hinzugef?gt

rfl_schedule_away_games <- latest_franchises %>%
  dplyr::select(franchise_id) %>% # hole alle team IDs aus latest franchises
  dplyr::left_join(rfl_schedules %>% dplyr::rename(opponent_2_id = franchise_id), by = c("franchise_id" = "opponent_id"), multiple = "all") %>% # standard schedule daten, wo das franchise away team ist, werden hinzugef?gt
  dplyr::rename(opponent_id = opponent_2_id)

rfl_matchups <- rbind(rfl_schedule_home_games, rfl_schedule_away_games) %>%
  dplyr::left_join(regularseason, by = c("season", "week", "franchise_id")) %>%
  dplyr::mutate(
    week = as.character(week),
    points_for = as.numeric(points_for)
  ) %>%
  dplyr::left_join(regularseason %>% dplyr::select(season, week, franchise_id, points_for) %>% dplyr::rename(points_against = points_for), by = c("season", "week", "opponent_id" = "franchise_id")) %>%
  dplyr::mutate(
    result = case_when(
      points_for > points_against ~ "W",
      points_for < points_against ~ "L",
      points_for == points_against ~ "T"
    ),
    win = ifelse(result == "W", 1, 0)
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    sf_aps = mean(points_for),
    sf_skill = points_for / sf_aps,
    sf_luck = sf_aps / points_for,
  ) %>%
  dplyr::group_by(franchise_id, season) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    wins_running = win[1] + cumsum(c(0, win[-1])),
    points_running = points_for[1] + cumsum(c(0, points_for[-1])),
    opp_points_running = points_against[1] + cumsum(c(0, points_against[-1])),
    aps_running = sf_aps[1] + cumsum(c(0, sf_aps[-1])),
    games = case_when(
      season >= 2017 & season <= 2020 ~ 12,
      TRUE ~ 13
    ),
    allplay_wins = sum(all_wins_week, na.rm = TRUE) / 2,
    allplay_winpct = allplay_wins / (games * 35),
    week = as.numeric(week),
    #phytEx = (points_running^2.37 / (points_running^2.37 + opp_points_running^2.37)) * (games * 2),
    #sf_phytEx = points_running^2.37 / (points_running^2.37 + aps_running^2.37),
    sf_expw = wins_running + ((((games * 2) - week) * points_running^2.37) / (points_running^2.37 + aps_running^2.37)),
    win_diff = wins_running - sf_expw
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    points_for_pctl = percent_rank(points_for),
    points_against_pctl = percent_rank(points_against),
    sf_skill_pctl = percent_rank(sf_skill),
    sf_luck_pctl = percent_rank(sf_luck),
    total_wins_pctl = percent_rank(wins_running),
    #phytEx_pctl = percent_rank(phytEx)
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    points_for_pctl_wkly = percent_rank(points_for),
    points_against_pctl_wkly = percent_rank(points_against),
    sf_skill_pctl_wkly = percent_rank(sf_skill),
    sf_luck_pctl_wkly = percent_rank(sf_luck),
    total_wins_pctl_wkly = percent_rank(wins_running),
    #phytEx_pctl_wkly = percent_rank(phytEx)
  ) %>%
  dplyr::ungroup()

rm(regularseason, rfl_schedules, rfl_schedule_home_games, rfl_schedule_away_games)
