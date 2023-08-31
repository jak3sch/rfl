rfl_roster <- jsonlite::read_json(paste0(var_mfl_api_base, "export?TYPE=rosters&L=", var_mfl_league_id, "&JSON=1"))$rosters$franchise %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(franchise_id = id) %>%
  tidyr::unnest(player) %>%
  tidyr::unnest_wider(player) %>%
  dplyr::rename(player_id = id) %>%
  dplyr::mutate(player_id = as.numeric(player_id))
