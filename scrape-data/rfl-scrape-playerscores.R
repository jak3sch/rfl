library(tidyverse)

var.season <- 2023
var.week <- 1

scores <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", var.season, "/export?TYPE=playerScores&L=63018&APIKEY=&W=", var.week, "&YEAR=&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1")) %>%
  purrr::pluck("playerScores", "playerScore") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::left_join(
    jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", var.season, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1")) %>%
      purrr::pluck("players", "player") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1),
    by = "id"
  ) %>%
  dplyr::mutate(season = var.season) %>%
  dplyr::select(season, week, id, name, position, team, score) %>%
  dplyr::rename(
    player_id = id,
    player_name = name,
    pos = position,
    points = score
  )

loadData <- readRDS(paste0("data/playerscores/rfl-playerscores-", var.season, ".rds")) %>%
  dplyr::filter(as.numeric(week) < var.week)

saveRDS(rbind(loadData, scores), paste0("data/playerscores/rfl-playerscores-", var.season, ".rds"))

rm(var.week, var.season, scores, loadData)
