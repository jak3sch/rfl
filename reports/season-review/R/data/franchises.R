# franchises
latest_franchises <- latest_league_data$franchises$franchise %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(franchise_id = id, franchise_name = name) %>%

  # add division data
  dplyr::left_join(
    latest_league_data$divisions$division %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(division_name = name) %>%
      dplyr::mutate(division_name = stringr::str_replace(division_name, "\\s[^ ]+$", "")), # entferne "Division" aus Name
    by = c("division" = "id")
  ) %>%

  # add conference names
  dplyr::left_join(
    latest_league_data$conferences$conference %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(conference_name = name),
    by = c("conference" = "id")
  )

latest_franchise_ids <- as.vector(t(latest_franchises %>% dplyr::select(franchise_id)))
