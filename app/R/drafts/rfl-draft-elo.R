source("R/drafts/rfl-drafts.R")

rfl_draft_elo <- reactive({
  rfl_drafts_data %>%
    dplyr::left_join(
      player_elo %>%
        dplyr::group_by(mfl_id) %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::filter(week == max(week)) %>%
        dplyr::ungroup() %>%
        dplyr::select(mfl_id, player_elo_post),
      by = "mfl_id"
    ) %>%
    dplyr::mutate(round = as.numeric(round)) %>%
    dplyr::filter(
      !is.na(mfl_id) &
      season >= input$draftEloSelectSeason[1] & season <= input$draftEloSelectSeason[2] &
        round >= input$draftEloSelectRounds[1] & round <= input$draftEloSelectRounds[2]
    ) %>%
    dplyr::filter(
      if(isTruthy(input$draftEloSelectPosition))
        position %in% input$draftEloSelectPosition
      else
        TRUE
    ) %>%
    dplyr::filter(
      if(isTruthy(input$draftEloSelectTeams))
        franchise %in% input$draftEloSelectTeams
      else
        TRUE
    ) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name),
      by = c("franchise" = "franchise_id")
    ) %>%
    dplyr::mutate(
      player_elo_post = ifelse(is.na(player_elo_post), 0, player_elo_post)
    ) %>%
    dplyr::select(franchise_name, season, round, pick, overall, player_name, position, team, player_elo_post) %>%
    dplyr::rename(
      "RFL Team" = franchise_name,
      Saison = season,
      DR = round,
      DP = pick,
      OVRL = overall,
      Spieler = player_name,
      Pos = position,
      Team = team,
      ELO = player_elo_post
    )
})

