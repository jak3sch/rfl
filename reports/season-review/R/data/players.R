rfl_players <- jsonlite::read_json(paste0(v_mfl_api_base, "export?TYPE=players&L=", v_mfl_league_id, "&APIKEY=&DETAILS=&SINCE=2016&PLAYERS=&JSON=1"))$players$player %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::mutate(
    position = dplyr::case_when(
      position %in% c("DT", "DE") ~ "DL",
      position %in% c("S", "CB") ~ "DB",
      TRUE ~ position
    )
  ) %>%
  dplyr::rename(mfl_id = id)

nfl_players <- nflreadr::load_players() %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(gsis_id, mfl_id),
    by = "gsis_id"
  ) %>%
  dplyr::select(-position) %>%
  dplyr::left_join(
    rfl_players %>%
      dplyr::select(mfl_id, position),
    by = "mfl_id"
  ) %>%
  dplyr::mutate(
    position = ifelse(is.na(position), position_group, position),
    mfl_id = as.numeric(mfl_id)
  )
