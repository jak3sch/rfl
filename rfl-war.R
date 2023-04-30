library(magrittr)
library(dplyr)

# https://www.fantasypoints.com/nfl/articles/season/2021/fantasy-war-part-1-theory

last_season = 2016

# helper ----
create_eligable <- function(df) {
  df %>% 
    # erstelle weekly fpts ranks
    dplyr::group_by(season, week, pos) %>%
    dplyr::arrange(dplyr::desc(start_pct), dplyr::desc(player_score)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>% 
    
    # erstelle eligable status (ist ein spieler im theoretischen optimalen lineup für die woche?)
    dplyr::mutate(
      # alle starter erhalten eligable status
      eligable = dplyr::case_when(
        pos %in% c("QB", "TE", "PK") & rank <= 12 ~ 1,
        pos %in% c("RB", "WR", "DL", "LB", "DB") & rank <= 24 ~ 1,
        TRUE ~ 0
      ),
      flex = dplyr::case_when(
        pos %in% c("RB", "WR", "TE") & eligable == 0 ~ "FLEX", # alle offense FLEX eligable positionen werden zu flex zusammengefasst
        pos %in% c("DL", "LB", "DB") & eligable == 0 ~ "IDP", # alle defense FLEX eligable positionen werden zu idp zusammengefasst
        TRUE ~ "no"
      )
    ) %>%
    dplyr::group_by(week, flex) %>%
    dplyr::arrange(dplyr::desc(start_pct), dplyr::desc(player_score)) %>%
    dplyr::mutate(rank_flex = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      eligable = dplyr::case_when(
        flex == "FLEX" & rank_flex <= 24 ~ 1, # die ersten 24 flex spots bekommen eligable status
        flex == "IDP" & rank_flex <= 36 ~ 1, # die ersten 24 IDP spots bekommen eligable status
        TRUE ~ eligable
      )
    )
}

# base data ----
starter <- purrr::map_df(2016:last_season, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
  )
})

total_games <- starter %>%
  dplyr::select(season, week) %>%
  dplyr::distinct() %>%
  dplyr::summarise(games = n()) %>% 
  dplyr::pull(games)

## average team weekly score ----
## 1. adding the individual scores of all nine starting positions together
## 2. we'll use the average points from the top-12 (24 for RB and WR) highest-percentage started players of each week
## 3. The process is the same for flex as a single starting position, except that all players considered a starter in their native position (RB, WR, TE) are not eligible for flex, as it's presumed these players already occupy a starting spot somewhere in the league. However, the next top-12 most-started combination of RB, WR, and TE for each week do qualify.
## 4. Simply adding all nine positional averages from this process together produces the expected amount of points the average team in a 12-team half-point PPR league for any given week.

starter_by_week <- starter %>%
  dplyr::filter(starter_status == "starter") %>% # filter nach allen Startern
  
  # fasse defensive positionen zusammen
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>% 
  
  # berechne start % innerhalb der liga
  dplyr::group_by(player_id, season, week, pos) %>%
  dplyr::mutate(start_pct = n() / 3) %>%
  dplyr::ungroup() %>% 
  dplyr::select(season, week, player_id, pos, start_pct, player_score) %>% 
  dplyr::distinct() %>% 
  
  create_eligable()
  

## avg team points & std deviation ----
avg_player <- starter_by_week %>%
  dplyr::filter(
    eligable == 1,
    !is.na(player_score)
  ) %>%
  dplyr::mutate(
    pos = ifelse(flex == "no", pos, flex)
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(
    points_average_player = mean(player_score),
    sd = stats::sd(player_score),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    multiplier = dplyr::case_when(
      pos %in% c("QB", "TE", "PK") ~ 1,
      pos %in% c("RB", "WR", "FLEX", "DL", "LB", "DB") ~ 2,
      pos == "IDP" ~ 3
    ),
    sd = dplyr::case_when(
      pos %in% c("QB", "TE", "PK") ~ sd^2,
      pos %in% c("RB", "WR", "FLEX", "DL", "LB", "DB") ~ sd^2 + sd^2,
      pos == "IDP" ~ sd^2 + sd^2 + sd^2
    )
  )

avg_team <- avg_player %>%
  dplyr::mutate(points_average_player = points_average_player * multiplier) %>%
  dplyr::summarise(
    points = sum(points_average_player),
    sd = sqrt(sum(sd))
  )

## replacement player ----
## The idea of a replacement player is that if you have a starting player miss a game due to injury, suspension, bye, etc., you are forced to insert your next best option into your starting lineup.
## This would either be someone on your bench or from the waiver wire/free agent pool.
replacement <- starter_by_week %>%
  filter(eligable == 0) %>%
  create_eligable() %>% 
  dplyr::filter(
    eligable == 1,
    !is.na(player_score)
  ) %>%
  dplyr::mutate(
    pos = ifelse(flex == "no", pos, flex)
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(
    points_replacement_player = mean(player_score)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(avg_player %>% dplyr::select(pos, points_average_player), by = "pos") %>%
  dplyr::mutate(
    replacement_team_points = avg_team$points - points_average_player + points_replacement_player,
    win_probability_replacement = pnorm(replacement_team_points, avg_team$points, sd = avg_team$sd),
    replacement_wins = total_games * win_probability_replacement
  ) %>%
  dplyr::select(-points_average_player)

## WAR berechnung ----
war <- starter %>%
  dplyr::select(season, week, player_id, player_name, player_score, pos) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(player_score)) %>%
  dplyr::group_by(player_id, player_name, season) %>%
  dplyr::summarise(
    points = sum(player_score),
    games = n(),
    .groups = "drop"
  ) %>%
  dplyr::group_by(player_id, player_name) %>%
  dplyr::mutate(
    points = sum(points),
    games_played = sum(games),
    games_missed = total_games - games,
    .groups = "drop"
  ) %>%
  dplyr::left_join(starter_by_week %>% dplyr::select(player_id, pos) %>% dplyr::distinct(), by = "player_id", multiple = "all", relationship = "many-to-many") %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("S", "CB") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::left_join(avg_player %>% dplyr::select(pos, points_average_player), by = "pos", multiple = "all", relationship = "many-to-many") %>%
  
  dplyr::mutate(
    points_per_game = points / games_played,
    points_average_player = points_average_player,
    avg_team_points = avg_team$points,
    war_team_points = avg_team_points - points_average_player + points_per_game,
    win_probability = pnorm(war_team_points, avg_team_points, sd = avg_team$sd), # pnorm "abritary normal distribution"; berechnet wahrscheinlichkeit aus gesuchtem wert, avg und standard deviation
  ) %>%
  dplyr::filter(!is.na(win_probability)) %>%
  dplyr::left_join(replacement, by = "pos", multiple = "all") %>%
  dplyr::group_by(player_id, player_name) %>%
  dplyr::summarise(
    points= sum(points),
    across(c(games_missed, win_probability, avg_team_points, win_probability_replacement, replacement_wins), mean),
    win_probability = ifelse(
      games_missed == 0, win_probability, (win_probability + (win_probability_replacement * games_missed)) / (games_missed + 1)
    ),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    expected_wins = total_games * win_probability, #
    war = round(expected_wins - replacement_wins, 2)
  ) %>%
  dplyr::left_join(starter_by_week %>% select(player_id, pos) %>% distinct(), by = "player_id", multiple = "all", relationship = "many-to-many") %>%
  dplyr::select(player_id, player_name, pos, points, war)

write.table(war, paste0("data/war/rfl-war-", last_season, ".csv"), row.names = FALSE, col.names = TRUE, sep = "; ")