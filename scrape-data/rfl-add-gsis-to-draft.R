library(nflreadr)
library(tidyverse)

rfl_drafts <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/drafts/rfl-draft.csv", col_types = "iicccccccci")

merge <- rfl_drafts %>% 
  dplyr::filter(is.na(gsis_id)) %>% 
  dplyr::select(-gsis_id) %>% 
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>% 
      dplyr::select(mfl_id, gsis_id),
    by = "mfl_id"
  ) %>% 
  dplyr::select(season:mfl_id, gsis_id, player_name:is_rookie)

new_data <- rbind(rfl_drafts %>% 
                    dplyr::filter(is.na(gsis_id)),
                  merge) %>% 
  dplyr::arrange(season, overall)

readr::write_csv(new_data, "data/drafts/rfl-draft.csv")
