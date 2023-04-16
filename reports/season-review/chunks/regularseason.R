# regular season ----
regularseason <- weeklyTotals %>%
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
schedulesClean <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/rfl-schedules.csv") %>%
  dplyr::mutate(season = as.numeric(season))

# durch double header matchups mit utnerschiedlichen home/away schedules muss erst einmal der spielplan normalisiert werden
homeMatches <- franchisesLatest %>%
  dplyr::select(franchise_id) %>% # hole alle team IDs aus latest franchises
  dplyr::left_join(schedulesClean, by = "franchise_id", multiple = "all") # standard schedule daten, wo das franchise home team ist, werden hinzugef?gt

awayMatches <- franchisesLatest %>%
  dplyr::select(franchise_id) %>% # hole alle team IDs aus latest franchises
  dplyr::left_join(schedulesClean %>% rename(opponent_2_id = franchise_id), by = c("franchise_id" = "opponent_id"), multiple = "all") %>% # standard schedule daten, wo das franchise away team ist, werden hinzugef?gt
  dplyr::rename(opponent_id = opponent_2_id)

matchups <- rbind(homeMatches, awayMatches) %>%
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

rm(regularseason, schedulesClean, homeMatches, awayMatches)
