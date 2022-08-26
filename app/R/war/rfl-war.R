# https://www.fantasypoints.com/nfl/articles/season/2021/fantasy-war-part-1-theory

# base data ----

## average team weekly score ----
## 1. adding the individual scores of all nine starting positions together
## 2. we'll use the average points from the top-12 (24 for RB and WR) highest-percentage started players of each week
## 3. The process is the same for flex as a single starting position, except that all players considered a starter in their native position (RB, WR, TE) are not eligible for flex, as it's presumed these players already occupy a starting spot somewhere in the league. However, the next top-12 most-started combination of RB, WR, and TE for each week do qualify.


## 4. Simply adding all nine positional averages from this process together produces the expected amount of points the average team in a 12-team half-point PPR league for any given week.

starterByWeek <- reactive({
  starter %>% 
    filter(
      starter_status == "starter",
      season >= input$selectSeason[1] && season <= input$selectSeason[2]
    ) %>% 
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
})

## avg team points & std deviation ----
avgPlayer <- reactive({
  starterByWeek() %>% 
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
})

avgTeam <- reactive({
  avgPlayer() %>% 
    mutate(points_average_player = points_average_player * multiplier) %>% 
    summarise(
      points = sum(points_average_player),
      sd = sqrt(sum(sd))
    )
})

## replacement player ----
## The idea of a replacement player is that if you have a starting player miss a game due to injury, suspension, bye, etc., you are forced to insert your next best option into your starting lineup.
## This would either be someone on your bench or from the waiver wire/free agent pool.
replacement <- reactive({
  starterByWeek() %>% 
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
    left_join(avgPlayer() %>% select(pos, points_average_player), by = "pos") %>% 
    mutate(
      replacement_team_points = avgTeam$points - points_average_player + points_replacement_player,
      win_probability_replacement = pnorm(replacement_team_points, avgTeam$points, sd = avgTeam$sd),
      replacement_wins = 13 * win_probability_replacement
    ) %>% 
    select(-points_average_player)
})

## WAR berechnung ----
war <- reactive({
  scores() %>% 
    select(-is_available) %>% 
    group_by(player_id) %>% 
    mutate(
      games = n(),
      games_missed = 13 - games,
      pos = case_when(
        pos %in% c("DT", "DE") ~ "DL",
        pos %in% c("S", "CB") ~ "CB",
        TRUE ~ pos
      )
    ) %>%
    ungroup() %>%
    left_join(avgPlayer() %>% select(pos, points_average_player), by = "pos") %>%
    mutate(
      avg_team_points = avgTeam$points,
      war_team_points = avg_team_points - points_average_player + points,
      win_probability = pnorm(war_team_points, avg_team_points, sd = avgTeam$sd), # pnorm "abritary normal distribution"; berechnet wahrscheinlichkeit aus gesuchtem wert, avg und standard deviation
    ) %>% 
    filter(!is.na(win_probability)) %>%
    left_join(replacement, by = "pos") %>% 
    group_by(player_id) %>% 
    summarise(
      points= sum(points),
      across(c(games_missed, win_probability, avg_team_points, win_probability_replacement, replacement_wins), mean),
      win_probability = (win_probability + (win_probability_replacement * games_missed)) / (games_missed + 1)
    ) %>% 
    ungroup() %>%
    mutate(
      expected_wins = 13 * win_probability,
      war = expected_wins - replacement_wins
    ) %>% 
    left_join(scores %>% select(player_id, player_name, pos), by = "player_id") %>% 
    distinct() %>% 
    select(player_id, player_name, pos, points, war)
})