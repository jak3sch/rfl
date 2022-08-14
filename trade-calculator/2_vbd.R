# VBD ----
## players ----
vbdPlayers <- stats %>% 
  group_by(pos, season) %>% 
  arrange(desc(points)) %>% 
  mutate(
    vbd = case_when(
      pos %in% c("QB", "TE", "PK") ~ ppg - ppg[12],
      pos == "DL" ~ ppg - ppg[24],
      pos == "RB" ~ ppg - ppg[30],
      pos %in% c("WR", "DB") ~ ppg - ppg[36],
      TRUE ~ ppg - ppg[48]
    )
  ) %>% 
  ungroup() %>% 
  
  # spieler, die seit 2017 mehrere positionen hatten (z.B. montez sweat) bekommen für den gesamten zeitraum die position ihrer letzten position
  #left_join(positionHelper, by = c("player_id" = "mfl_id")) %>% 
  group_by(player_id) %>% 
  
  mutate(
    season_max_helper = max(season),
    last_pos_helper = ifelse(season == season_max_helper, pos, NA),
  ) %>% 
  arrange(last_pos_helper) %>% 
  tidyr::fill(last_pos_helper) %>% 
  mutate(
    pos = last_pos_helper,
  ) %>% 
  select(-ends_with("_helper")) %>% 
  ungroup() 

## draft picks ----
vbdPicksRaw <- drafts %>% 
  select(overall, player_id, player_name) %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  left_join(
    vbdPlayers %>%
      select(player_id, vbd) %>% 
      group_by(player_id) %>% 
      summarise(vbd = mean(vbd), .groups = "drop"),
    by = "player_id"
  ) %>% 
  filter(!is.na(vbd)) %>% 
  group_by(overall) %>% 
  summarise(vbd_pick_avg = mean(vbd))

### Calc of smooth values
vbdPicks.lo <- loess(vbd_pick_avg ~ overall, vbdPicksRaw)
vbdPicks.pred <- predict(vbdPicks.lo, data.frame(overall = seq(1, 252, 1)), se = TRUE)$fit

pickValue <- as.data.frame(vbdPicks.pred) %>% 
  rename(pick_value_raw = vbdPicks.pred) %>% 
  mutate(
    overall = row_number(),
    pick_value_raw = round(as.numeric(pick_value_raw), 4)
  )

write.csv(pickValue, "pickvalue.csv")
