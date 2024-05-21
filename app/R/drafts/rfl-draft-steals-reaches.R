draft_steals_reaches <- rfl_drafts_data %>%
  dplyr::filter(is_rookie == 1) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::mutate(
    min_pick = min(overall),
    max_pick = max(overall),
    avg_pick = mean(overall),
    sd_pick = sd(overall)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    diff_min = overall - min_pick,
    diff_max = overall - max_pick,
    diff_avg = overall - avg_pick,
    #diff_sd = overall - sd_pick,
    mean = (diff_min + diff_max + diff_avg) / 3,
    score = mean - sd_pick
  ) %>%
  #dplyr::select(-is_rookie, -dplyr::ends_with("_pick")) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = c("franchise" = "franchise_id")
  )

