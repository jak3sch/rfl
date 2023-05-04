league_position_mvps <- war %>%
  dplyr::filter(season == 2022) %>%
  dplyr::distinct() %>%
  dplyr::left_join(nflreadr::load_ff_playerids() %>% dplyr::mutate(mfl_id = as.double(mfl_id)) %>% dplyr::select(mfl_id, gsis_id), by = c("player_id" = "mfl_id")) %>%
  dplyr::left_join(nflreadr::load_players() %>% dplyr::select(gsis_id, display_name), by = "gsis_id") %>%
  dplyr::filter(!is.na(war)) %>%
  dplyr::group_by(pos) %>%
  dplyr::arrange(desc(war)) %>%
  dplyr::mutate(
    rank = dplyr::row_number(),
    war_pct = war / max(war),
    colHeight = 1 * war_pct,
    pos_f = factor(pos, c("QB", "RB", "WR", "TE", "PK", "DL", "LB", "DB"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(rank <= 3)



chart <- ggplot(subset(league_mvp_positions, rank <= 3), aes(x = factor(rank, levels = c(2,1,3)), y = colHeight)) +
  plotDefaultsMinimal +
  facet_wrap(~pos_f, ncol = 5, strip.position = "bottom") +
  geom_col(aes(alpha = rank), fill = var.colorAccent) +

  geom_text(aes(label = war), vjust = 1.7, size = plot.geom_text_large_size, color = var.colorBlue, family = plot.geom_text_large_family) +
  geom_text(aes(label = paste0("#", rank)), vjust = 4.5, color = var.colorBlue, size = plot.geom_text_small_size, family = plot.geom_text_small_family) +

  nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), y = 0, height = 0.25, vjust = 0) +
  geom_text(aes(label = sapply(display_name, function(x) paste(strwrap(x, width = 5), collapse = "\n"))), vjust = -0.5, hjust = 0.5, size = plot.geom_text_xsmall_size, family = plot.geom_text_xsmall_family, color = var.colorAccent) +

  geom_hline(yintercept = 0, color = var.colorAccent, size = plot.line_width_m) +

  scale_alpha_continuous(range = c(0.5, 0.3)) +
  scale_y_continuous(limits = c(0, 1.2)) +
  labs(
    title = toupper(paste("Die wertvollsten Spieler", var.lastSeason)),
    subtitle = "Die nach WAR wertvollsten Spieler ihrer Positionsgruppe.",
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.text = element_text(color = plot.theme_text_shadow_color, size = plot.theme_text_shadow_size, family = plot.theme_text_shadow_family),
    panel.spacing = unit(1, "lines")
  )
