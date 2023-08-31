# data ----
rfl_drafts <- purrr::map_df(seq(var_season_first + 1, var_season_last), function(x) {
  jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", x, "/export?TYPE=draftResults&L=", var_mfl_league_id, "&JSON=1"))$draftResults$draftUnit$draftPick %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::mutate(season = x)
}) %>%
  dplyr::mutate(
    player = as.numeric(player)
  ) %>%
  dplyr::left_join(
    war %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(
        points = sum(points, na.rm = TRUE),
        career_war = round(sum(war, na.rm = TRUE), 2)
      ),
    by = c("player" = "player_id"),
    multiple = "all"
  )

rfl_draft_data <- rfl_drafts %>%
  dplyr::select(-comments, -timestamp) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(round, pick) %>%
  dplyr::mutate(
    overall = dplyr::row_number(),
    overall_clean = ceiling(overall / 3), # runde overall pick immer zur n?chst h?heren, vollen zahl auf
    is_bust = ifelse(career_war <= 0 | is.na(career_war), 1, 0)
  ) %>%
  dplyr::arrange(dplyr::desc(career_war)) %>%
  dplyr::mutate(
    rank_by_war = dplyr::row_number()
  ) %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(
    pick_min = min(overall),
    pick_max = max(overall),
    pick_avg = mean(overall),
    rank_diff = rank_by_war - pick_avg
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    latest_franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = c("franchise" = "franchise_id")
  ) %>%
  dplyr::left_join(
    nfl_players %>%
      dplyr::filter(!is.na(mfl_id)) %>%
      dplyr::select(mfl_id, display_name, position),
    by = c("player" = "mfl_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::distinct()

rfl_draft_hit_rates <- rfl_draft_data %>%
  dplyr::group_by(season, franchise_name, is_bust) %>%
  dplyr::summarise(picks = n(), .groups = "drop") %>%
  dplyr::group_by(season, franchise_name) %>%
  dplyr::mutate(
    total_picks = sum(picks),
    hits = ifelse(is_bust == 0, picks, total_picks - picks),
    hit_rate = hits / total_picks
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-is_bust, -picks) %>%
  dplyr::distinct()
