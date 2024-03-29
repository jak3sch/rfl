war <- purrr::map_df(v_season_first:v_season_last, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-{x}.csv"),
    col_types = "iiccdd"
  ) %>%
    dplyr::mutate(season = x)
}) %>%
  # pctl
  dplyr::mutate(
    points_total_pctl = dplyr::percent_rank(points),
    war_total_pctl = dplyr::percent_rank(war)
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::mutate(
    points_pos_pctl = dplyr::percent_rank(points),
    war_pos_pctl = dplyr::percent_rank(war)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(
    points_season_pctl = dplyr::percent_rank(points),
    war_season_pctl = dplyr::percent_rank(war)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    nfl_players %>%
      dplyr::select(mfl_id, display_name),
    by = c("player_id" = "mfl_id")
  )
