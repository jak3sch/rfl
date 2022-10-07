elo <- read.csv("data/rfl-elo.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character")) %>% 
  left_join(franchises %>% select(franchise_id, franchise_name, division_name), by = "franchise_id") %>% 
  left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

running_elo <- elo %>% 
  group_by(franchise_id) %>%
  mutate(game = paste0(season, week)) %>%
  arrange(game) %>% 
  select(game, franchise_id, franchise_elo_postgame) %>% 
  distinct() %>% 
  mutate(game = row_number()) %>% 
  left_join(franchises %>% select(franchise_id, franchise_name, division_name), by = "franchise_id") %>% 
  ungroup()

elo_upsets <- elo %>% 
  mutate(
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_differential > 0, 1, 0)
  ) %>% 
  filter(upset == 1) %>%
  select(season, week, franchise_name, franchise_elo_pregame, franchise_score, elo_diff, opponent_score, opponent_name, opponent_elo_pregame) %>% 
  rename(
    Season = season,
    WK = week,
    Home = franchise_name,
    Away = opponent_name,
    "Home ELO" = franchise_elo_pregame,
    "Away ELO" = opponent_elo_pregame,
    "ELO Diff" = elo_diff,
    "Home Score" = franchise_score,
    "Away Score" = opponent_score
  )

elo_matchups <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", var.season, "/export?TYPE=schedule&L=63018&APIKEY=&W=", nflreadr::get_current_week(), "&JSON=1")) %>% 
  purrr::pluck("schedule", "weeklySchedule", "matchup") %>% 
  tibble::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest_wider(1) %>% 
  rename(franchise_id = id) %>%
  select(-spread, -isHome, -result) %>% 
  tidyr::unnest_wider(2) %>% 
  rename(opponent_id = id) %>%
  select(franchise_id, opponent_id) %>% 
  left_join(
    elo %>% 
      filter(season == var.season & week == nflreadr::get_current_week() -1) %>% 
      select(franchise_id, franchise_elo_pregame) %>% rename(franchise_elo = franchise_elo_pregame),
    by = "franchise_id"
  ) %>% 
  left_join(
    elo %>% 
      filter(season == var.season & week == nflreadr::get_current_week() -1) %>% 
      select(franchise_id, franchise_elo_pregame) %>% rename(opponent_elo = franchise_elo_pregame),
    by = c("opponent_id" = "franchise_id")
  ) %>% 
  distinct() %>% 
  mutate(win_pct = 1 / (10^(-(franchise_elo - opponent_elo) / 400 ) + 1)) %>% 
  left_join(franchises %>% select(franchise_id, franchise_name), by = "franchise_id") %>% 
  left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id")) %>% 
  mutate("ELO Adv" = scales::percent(win_pct, 1)) %>% 
  select(franchise_name, franchise_elo, "ELO Adv", opponent_elo, opponent_name) %>%
  rename(Home = franchise_name, "Home ELO" = franchise_elo, "Away ELO" = opponent_elo, Away = opponent_name)
