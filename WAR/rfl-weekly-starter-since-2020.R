library(ffscrapr)
library(tidyverse)

rfl <- ffscrapr::mfl_connect(league_id = "63018", season = 2021)
starters <- ffscrapr::ff_starters(rfl, season = 2021, weeks = 1:13) %>% 
  select(season, week, franchise_id, starter_status, player_id, should_start, player_score, player_name, pos, team) %>% 
  distinct()

utils::write.csv(starters, here("data/starter/rfl-starter-2021.csv"), row.names = F)
