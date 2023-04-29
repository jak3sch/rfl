starter <- purrr::map_df(2016:var.lastSeason, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
  )
}) %>%
  dplyr::left_join(latest_franchises %>% select(franchise_id, franchise_name), by = "franchise_id")