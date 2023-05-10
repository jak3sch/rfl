rfl_drafts <- purrr::map_df(seq(var_season_first + 1, var_season_last), function(x) {
  jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", x, "/export?TYPE=draftResults&L=", var_mfl_league_id, "&JSON=1"))$draftResults$draftUnit$draftPick %>% 
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>% 
    dplyr::mutate(season = x)
})
  
rfl_draft_data <- rfl_drafts %>% 
  dplyr::mutate(
    player = as.numeric(player)
  ) %>% 
  dplyr::left_join(
    war %>% 
      dplyr::select(-pos, -points, -player_name) %>% 
      dplyr::filter(season >= var_season_first + 1) %>% 
      dplyr::mutate(season = paste0("s_", season)) %>% 
      tidyr::spread(season, war),
    by = c("player" = "player_id"),#
    multiple = "all"
  ) %>% 
  dplyr::group_by(season) %>% 
  dplyr::arrange(round, pick) %>% 
  dplyr::mutate(
    overall = dplyr::row_number(),
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(war_total = sum(across(starts_with("s_")), na.rm = TRUE)) %>% 
  dplyr::group_by(franchise) %>% 
  dplyr::mutate(
    franchise_war_all_drafts = sum(war_total),
    franchise_war_per_pick_all_drafts = franchise_war_all_drafts / n()
  ) %>% 
  dplyr::select(-comments, -timestamp)

war_helper <- function(df, years) {
  df %>% 
    dplyr::filter(season >= var_season_last - {{years}}) %>% 
    dplyr::group_by(franchise) %>% 
    dplyr::summarise(
      franchise_war_drafts = sum(war_total),
      franchise_war_per_pick_drafts = franchise_war_drafts / n()
    ) %>% 
    dplyr::arrange(dplyr::desc(franchise_war_per_pick_drafts)) %>% 
    dplyr::mutate(
      franchise_war_per_pick_drafts_rank = dplyr::row_number(),
      #franchise_war_per_pick_drafts_pctl = dplyr::percent_rank(franchise_war_per_pick_drafts)
    ) %>% 
    dplyr::rename_with(~paste0(., "_", {{years}}), dplyr::starts_with("franchise_"))
}
  
rfl_draft_teams <- war_helper(rfl_draft_data, 5) %>% 
  dplyr::left_join(
    war_helper(rfl_draft_data, 3),
    by = "franchise"
  ) %>% 
  dplyr::left_join(
    war_helper(rfl_draft_data, var_season_last - var_season_first + 1),
    by = "franchise"
  ) %>% 
  dplyr::left_join(
    latest_franchises %>% 
      dplyr::select(franchise_id, franchise_name),
    by = c("franchise" = "franchise_id")
  )

ggplot(rfl_draft_data, aes(x = overall, y = war_total)) +
  geom_point(data = subset(rfl_draft_data, season != var_season_last - 3), color = "grey") +
  geom_point(data = subset(rfl_draft_data, season == var_season_last - 3), color = "black") +
  geom_point(data = subset(rfl_draft_data, franchise == "0007" & season == var_season_last - 3), color = "red", size = 8)
  geom_text()

ggplot(rfl_draft_teams, aes(x = franchise_war_per_pick_drafts_7, y = reorder(franchise_name, franchise_war_per_pick_drafts_7))) +
  geom_col() +
  labs(
    title = paste0(var_season_last - 7, "-", var_season_last),
    x = "WAR per Draftpick",
    y = ""
  )
