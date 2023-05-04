war <- purrr::map_df(var_season_first:var_season_last, function(x) {
  readr::read_delim(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-{x}.csv"),
    delim = "; "
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
  dplyr::ungroup()
