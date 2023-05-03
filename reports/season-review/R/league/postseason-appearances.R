# postseason data
postseason_list <- list()
# postseason data
postseason <- purrr::map_df(2017:var_season_last, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/weeklyResults/rfl-results-{x}-postseason.csv")
  )
}) %>%
  dplyr::group_by(season, bowl, franchise_id) %>%
  dplyr::mutate(
    appearance = ifelse(week == min(week), 1, 0)
  ) %>%
  dplyr::group_by(bowl, franchise_id) %>%
  dplyr::arrange(season) %>%
  dplyr::mutate(
    total_appearances = sum(appearance),
    next_appearance = dplyr::lead(season)
  ) %>%
  dplyr::select(-appearance) %>%
  dplyr::left_join(
    latest_franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  )

super_bowl <- postseason %>%
  dplyr::filter(
    bowl == "SB" & total_appearances > 1
  ) %>%
  dplyr::group_by(franchise_name, season) %>%
  dplyr::arrange(desc(po_finish)) %>%
  #dplyr::filter(row_number() == 1)
  dplyr::mutate(
    result_color = dplyr::case_when(
      po_finish == 1 ~ "1st",
      po_finish == 2 ~ "2nd",
      po_finish == 3 ~ "3rd",
      TRUE ~ "4th +"
    ),
    result_color = factor(result_color, levels = c("1st", "2nd", "3rd", "4th +"))
  ) %>%
  dplyr::select(season, franchise_name, total_appearances, po_finish, result_color, next_appearance)

rm(tmp)

write_csv(postseason, "data.csv")
write_csv(super_bowl, "super_bowl.csv")
