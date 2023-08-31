league_position_mvps <- war %>%
  dplyr::filter(season == var_season_last) %>%
  dplyr::distinct() %>%
  dplyr::left_join(nflreadr::load_ff_playerids() %>% dplyr::mutate(mfl_id = as.double(mfl_id)) %>% dplyr::select(mfl_id, gsis_id), by = c("player_id" = "mfl_id")) %>%
  #dplyr::left_join(nflreadr::load_players() %>% dplyr::select(gsis_id, display_name), by = "gsis_id") %>%
  dplyr::filter(!is.na(war)) %>%
  dplyr::group_by(pos) %>%
  dplyr::arrange(desc(war)) %>%
  dplyr::mutate(
    rank = dplyr::row_number(),
    war_pct = war / max(war),
    colHeight = 1 * war_pct,
    pos_f = factor(pos, var_fantasy_pos_order)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(rank <= 3)

position_mvps_plot <- ggplot2::ggplot(league_position_mvps, aes(x = factor(rank, levels = c(2,1,3)), y = colHeight)) +
  ggplot2::facet_wrap(~pos_f, ncol = 4, strip.position = "bottom") +
  ggplot2::geom_col(aes(fill = pos)) +

  ggplot2::labs(
    title = paste0("Die wertvollsten Spieler ", var_season_last),
    subtitle = "Die Grafik zeigt die Spieler mit den meisten WAR in ihrer Positionsgruppe."
  ) +

  ggplot2::scale_y_continuous(limits = c(0, 1.2)) +
  plot_defaults_mvp_bar +
  default_plot_minimal +
  ggplot2::theme(
    legend.position = "none",
    strip.text = ggplot2::element_text(vjust = 1, hjust = 0.5, color = color_light, family = "accent", size = 20, lineheight = 0.35),
    panel.spacing = ggplot2::unit(3, "mm")
  )
