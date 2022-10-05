library(ffscrapr)
library(tidyverse)
library(nflreadr)

rfl <- ffscrapr::mfl_connect(league_id = "63018", season = 2022)
starters <- ffscrapr::ff_starters(rfl, season = 2022) %>% 
  select(season, week, franchise_id, starter_status, player_id, should_start, player_score, player_name, pos, team) %>% 
  filter(week <= nflreadr::get_current_week() - 1) %>% 
  distinct()

utils::write.csv(starters, "data/starter/rfl-starter-2022.csv", row.names = F)

rm(rfl, starters)
