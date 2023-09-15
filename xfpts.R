library(tidyverse)
library(nflreadr)

rules <- jsonlite::read_json("https://www45.myfantasyleague.com/2023/export?TYPE=rules&L=54277&APIKEY=aRNp3s%2BWvuWsx12mPlrBYDoeErox&JSON=1")$rules$positionRules %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::unnest(rule) %>%
  tidyr::unnest_wider(rule) %>%
  tidyr::unnest(event) %>%
  tidyr::unnest(points) %>%
  tidyr::unnest(range) %>%
  dplyr::mutate(
    event = as.character(event),
    points = as.numeric(stringr::str_remove(points, "[*]")),
    range = stringr::str_split(range, "(?<!^)-"),
    range_min = purrr::map_chr(range, 1),
    range_max = purrr::map_chr(range, 2),
    position = stringr::str_split(positions, "\\|")
  ) %>%
  dplyr::select(event, points, range_min, range_max, position) %>%
  dplyr::filter(!event %in% c("FG", "MG", "EP", "EM", "#UT", "UY", "#KT", "KY", "#BF", "BLF", "#MF", "#BP", "BLP", "BLE", "#M", "#N", "PNY", "PI", "HBP")) %>% # keine ST stats
  tidyr::unnest(position)

offense_stats <- nflreadr::load_ff_opportunity(stat_type = "weekly") %>%
  # rename stats to mfl rules: https://api.myfantasyleague.com/2023/export?TYPE=allRules&JSON=1
  dplyr::left_join(
    nflreadr::load_player_stats(stat_type = "offense") %>%
      dplyr::select(player_id, week, sacks, sack_yards),
    by = c("player_id", "week")
  ) %>%
  dplyr::mutate(
    FL = rec_fumble_lost + rush_fumble_lost,
    FUO = FL
  ) %>%
  dplyr::rename(
    PA = pass_attempt,
    PC = pass_completions_exp,
    "#P" = pass_touchdown_exp,
    PY = pass_yards_gained_exp,
    P2 = pass_two_point_conv_exp,
    RA = rush_attempt,
    "#R" = rush_touchdown_exp,
    RY = rush_yards_gained_exp,
    R2 = rush_two_point_conv_exp,
    TGT = rec_attempt,
    CC = receptions_exp,
    "#C" = rec_touchdown_diff,
    CY = rec_yards_gained_exp,
    C2 = rec_two_point_conv_exp,
    TSK = sacks,
    TSY = sack_yards,
    #FC fumble recoveries nicht in stats enthalten
    # OFC own fumble recoveries nicht in stats enthalten
    # OFCY Own Fumble Recovery Yardage
    IN = pass_interception_exp,
    FD = total_first_down_exp,
    # FY penalty yds
    # "#FR" offense fumble recovery tds nicht in stats enthalten
  ) %>%

  tidyr::gather(
    event, value, dplyr::one_of(unique(rules$event))
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
    avg_qb_hit_pct = def_qb_hits / defense_snaps,
    avg_sacks_pct = def_sacks / defense_snaps,
    avg_sack_yds = mean(def_sack_yards, na.rm = TRUE),
    avg_fumbles_forced_pct = def_fumbles_forced / defense_snaps,
    avg_int_return_yds = mean(def_interception_yards, na.rm = TRUE),
    avg_fumble_return_yds = mean(def_fumble_recovery_yards_opp, na.rm = TRUE),
    avg_safety_pct = def_safety / defense_snaps,
    avg_td_pct = def_tds / defense_snaps,
  )

defense_stats <- nflreadr::load_player_stats(stat_type = "defense") %>%
  #dplyr::select(season, week, player_id, player_name, position, position_group) %>%
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

  # defense avg
  dplyr::left_join(
    defense_benchmarks %>%
      dplyr::group_by(position_group) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with("_pct"), \(x) mean(x, na.rm = TRUE)),
        dplyr::across(dplyr::ends_with("_yds"), \(x) mean(x, na.rm = TRUE))
      ),
    by = "position_group"
  ) %>%

# player avg
  dplyr::left_join(
    defense_benchmarks %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(
        dplyr::across(c(def_tackles:def_interceptions, def_pass_defended, defense_snaps), \(x) sum(x, na.rm = TRUE))
      ) %>%
      dplyr::mutate(
        player_avg_tackle_solo_pct = def_tackles_solo / defense_snaps,
        player_avg_tackle_assist_pct = def_tackles_with_assist / defense_snaps,
        player_avg_tackles_for_loss_pct = def_tackles_for_loss / defense_snaps,
        player_avg_interceptions_pct = def_interceptions / defense_snaps,
        player_avg_pass_defended_pct = def_pass_defended / defense_snaps,
        player_avg_qb_hit_pct = def_qb_hits / defense_snaps,
        player_avg_sacks_pct = def_sacks / defense_snaps,
        player_avg_fumbles_forced_pct = def_fumbles_forced / defense_snaps,
      ) %>%
      dplyr::select(player_id, dplyr::starts_with("player_avg")),
    by = "player_id"
  ) %>%

  # exp stats erstellen
  dplyr::mutate(
    TKD = defense_snaps * (avg_tackle_solo_pct + (player_avg_tackle_solo_pct - avg_tackle_solo_pct)),
    TK = TKD, # tackles on offense nicht in stats enthalten
    ASD = defense_snaps * (avg_tackle_assist_pct + (player_avg_tackle_assist_pct - avg_tackle_assist_pct)),
    AS = ASD, # assists on offense nicht in stats enthalten
    IC = defense_snaps * (avg_interceptions_pct + (player_avg_interceptions_pct - avg_interceptions_pct)),
    PD = defense_snaps * (avg_pass_defended_pct + (player_avg_pass_defended_pct - avg_pass_defended_pct)),
    QH = defense_snaps * (avg_qb_hit_pct + (player_avg_qb_hit_pct - avg_qb_hit_pct)),
    SK = defense_snaps * (avg_sacks_pct + (player_avg_sacks_pct - avg_sacks_pct)),
    FF = defense_snaps * (avg_fumbles_forced_pct + (player_avg_fumbles_forced_pct - avg_fumbles_forced_pct)),
    TKL = defense_snaps * (avg_tackles_for_loss_pct + (player_avg_tackles_for_loss_pct - avg_tackles_for_loss_pct)),
    SF = defense_snaps * avg_safety_pct,
    "#D" = defense_snaps * avg_td_pct
  ) %>%
  dplyr::rename(
    FUD = def_fumbles,
    FCY = avg_fumble_return_yds,
    ICY = avg_int_return_yds,
    SKY = avg_sack_yds,
    # D2 defenseretruned conversions nicht in stats enthalten
    # SF1safety for 1 point nicht in stats enthalten
    # "#DR" defense fumble recovery td nicht in stats enthalten - ist aber indefense tds dabei
    # "#IR" interception return td nicht in stats enthalten - ist aber indefense tds dabei
  ) %>%
  #dplyr::select(-dplyr::ends_with("_pct")) %>%
  tidyr::gather(
    event, value, dplyr::one_of(unique(rules$event))
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
  dplyr::summarise(xfpts = round(sum(xfpts_event, na.rm = TRUE), 2), .groups = "drop")


