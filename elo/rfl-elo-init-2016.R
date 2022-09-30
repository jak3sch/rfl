scores <- jsonlite::read_json("https://www48.myfantasyleague.com/2016/export?TYPE=weeklyResults&L=63018&APIKEY=&W=1&JSON=1") %>% 
  purrr::pluck("weeklyResults", "franchise") %>% 
  tibble::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  select(id, score) %>% 
  rename(franchise_id = id) %>% 
  mutate(score = as.numeric(score))


s2016 <- schedule %>% 
  filter(season == 2016, week == 1) %>% 
  
  # für jedes spiel zwei zeilen erstellen mit jedem team als franchise und opponent
  mutate(
    game_id = paste0(season, week, franchise_id, opponent_id),
    away_opponent = opponent_id,
    home_opponent = franchise_id
  ) %>% 
  gather(key, value, ends_with("_opponent")) %>% 
  mutate(
    opponent_id = as.character(ifelse(opponent_id == value, franchise_id, opponent_id)),
    franchise_id = as.character(ifelse(franchise_id == opponent_id, value, franchise_id)),
  ) %>% 
  select(game_id, season:opponent_id) %>% 
  distinct() %>% 
  
  # punkte
  left_join(scores %>% rename(franchise_score = score), by = c("franchise_id")) %>% 
  left_join(scores %>% rename(opponent_score = score), by = c("opponent_id" = "franchise_id")) %>% 
  
  mutate(
    score_differential = round(franchise_score - opponent_score, 2),
    result = case_when(
      score_differential > 0 ~ 1, # 1 für win
      score_differential < 0 ~ 0, # 0 für loss
      T ~ 0.5 # 0.5 für tie
    ), 
    franchise_elo_pregame = 1500,
    opponent_elo_pregame = 1500,
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
  select(1:8, franchise_elo_pregame, opponent_elo_pregame, elo_shift, franchise_elo_postgame)

write.csv(s2016, "data/elo/rfl-elo-init.csv", row.names = F)
