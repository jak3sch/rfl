library(tidyverse)

var.week <- 17

scores <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/2022/export?TYPE=playerScores&L=63018&APIKEY=&W=", var.week, "&YEAR=&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1")) %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::left_join(
    jsonlite::read_json("https://www55.myfantasyleague.com/2022/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1") %>%
      purrr::pluck("players", "player") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1),
    by = "id"
  ) %>%
  dplyr::mutate(season = 2022) %>%
  dplyr::select(season, week, id, name, position, team, score) %>%
  dplyr::rename(
    player_id = id,
    player_name = name,
    pos = position,
    points = score
  )

loadData <- readRDS("data/playerscores/rfl-playerscores-2022.rds") %>%
  dplyr::filter(as.numeric(week) < var.week)

saveRDS(rbind(loadData, scores), "data/playerscores/rfl-playerscores-2022.rds")

