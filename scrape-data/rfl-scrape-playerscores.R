library(tidyverse)
library(ffscrapr)

conn <- ffscrapr::ff_connect(platform = "mfl", league_id = 63018, 2022)
scores <- ffscrapr::ff_playerscores(conn, 2022, 1:3)

saveRDS(scores, "data/playerscores/rfl-playerscores-2022.rds")

loadData = readRDS("data/playerscores/rfl-playerscores-2022.rds")