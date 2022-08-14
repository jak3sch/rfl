library(tidyverse)
library(ffscrapr)

## Stats und Drafts ----
statsList <- list()
statsAvgList <- list()
draftList <- list()

for (i in 2017:2021) {
  connection <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = i, rate_limit = FALSE)
  statsTotal <- ffscrapr::ff_playerscores(connection, season = i, week = "YTD")
  statsAvg <- ffscrapr::ff_playerscores(connection, season = i, week = "AVG") %>% select(player_id, season, points) %>% rename(ppg = points)
  draft <- ffscrapr::ff_draft(connection) %>% mutate(season = 2017)
  
  statsList[[i]] <- statsTotal
  statsAvgList[[i]] <- statsAvg
  draftList[[i]] <- draft
}

stats <- do.call(rbind, statsList) %>% 
  left_join(do.call(rbind, statsAvgList), by = c("player_id", "season")) %>% 
  mutate(player_id = as.integer(player_id))

drafts <- do.call(rbind, draftList)
