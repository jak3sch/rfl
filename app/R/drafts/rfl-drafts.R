rfl_drafts <- reactive({
  rfl_drafts <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/drafts/rfl-draft.csv", col_types = "iicccccccci") %>%
    dplyr::filter(
      !is.na(position),
      #overall >= input$selectPicks[1] & overall <= input$selectPicks[2]
    ) %>%
    dplyr::group_by(season, mfl_id) %>%
    dplyr::arrange(overall) %>%
    dplyr::mutate(
      first_pick = overall[1],
      second_pick = overall[2],
      third_pick = overall[3]
    ) %>%
    dplyr::group_by(mfl_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(season, overall, round, mfl_id, position, first_pick, second_pick, third_pick) %>%
    dplyr::group_by(season, position) %>%
    dplyr::arrange(first_pick) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::ungroup()
})
