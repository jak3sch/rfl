library(tidyverse)

elo <- purrr::map_df(2016:2023, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-player-elo-{x}.csv")
  )
})

league_drafts_raw <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/drafts/rfl-draft.csv", col_types = "iicccdcccci")

pick_value <- league_drafts_raw %>%
  dplyr::left_join(
    elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::filter(player_elo_post == max(player_elo_post)) %>%
      dplyr::ungroup() %>%
      dplyr::select(mfl_id, player_elo_post),
    by = "mfl_id",
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(player_elo_post = ifelse(is.na(player_elo_post), 1400, player_elo_post)) %>% # wenn keine elo vorhanden, setze sie sehr niedrig
  dplyr::group_by(round, pick) %>%
  dplyr::summarise(avg_max_elo = round(mean(player_elo_post)), .groups = "drop") %>%
  dplyr::arrange(desc(avg_max_elo)) %>%
  dplyr::mutate(
    overall = row_number(),
    overall_clean = ceiling(overall / 3),
  ) %>%
  dplyr::group_by(overall_clean) %>%
  dplyr::mutate(avg_max_elo = max(avg_max_elo)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    round = ceiling(overall / 36),
    pick_value = round((avg_max_elo - min(avg_max_elo)) / (max(avg_max_elo) - min(avg_max_elo)), 2) * 100
  ) %>%
  dplyr::group_by(round) %>%
  dplyr::arrange(overall) %>%
  dplyr::mutate(
    pick = row_number(),
    pick_id = paste("DP", round, pick, sep = "_")
  ) %>%
  dplyr::ungroup()

# ToDo Std Abweichung für pick value berechnen, da nicht immer die 3 spieler hintereinander gepickt werden
