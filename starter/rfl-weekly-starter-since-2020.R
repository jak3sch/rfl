library(ffscrapr)
library(tidyverse)

rfl <- ffscrapr::mfl_connect(league_id = "63018", season = 2022)
starters <- ffscrapr::ff_starters(rfl, season = 2022) %>% 
  select(season, week, franchise_id, starter_status, player_id, should_start, player_score, player_name, pos, team) %>% 
  filter(week <= 3) %>% 
  distinct()

utils::write.csv(starters, here("data/starter/rfl-starter-2022.csv"), row.names = F)

rm(rfl, starters)
