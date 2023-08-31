# drafted positions ----
drafted_positions_plot <- ggplot2::ggplot(rfl_draft_data, ggplot2::aes(x = season, fill = position)) +
  ggplot2::facet_wrap(~round, ncol = 3) +
  ggplot2::geom_bar() +

  ggplot2::scale_fill_manual(values = colors_position, na.value = color_muted) +

  ggplot2::labs(
    title = "Gedraftete Positionen je Runde",
    x = "Runde",
    fill = ""
  ) +

  default_plot +
  ggplot2::theme(
    legend.position = "top"
  )

# hit rates ----
rfl_draft_hit_rates_order <- rfl_draft_hit_rates %>%
  dplyr::filter(season == var_season_last - 3) %>%
  dplyr::arrange(dplyr::desc(hit_rate)) %>%
  dplyr::pull(franchise_name) %>%
  dplyr::union(rfl_draft_hit_rates$franchise_name)

draft_hit_rates_plot <- ggplot2::ggplot(data = subset(rfl_draft_hit_rates, season <= var_season_last - 3), ggplot2::aes(x = hit_rate, y = factor(franchise_name, levels = rev(rfl_draft_hit_rates_order)))) +
  ggplot2::geom_rect(xmin = -0.25, xmax = -0.03, ymin = 0, ymax = 36.5, fill = color_bg) +
  ggplot2::geom_vline(xintercept = mean(rfl_draft_hit_rates$hit_rate), color = color_light, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_point(data = subset(rfl_draft_hit_rates, season < var_season_last - 3), mapping = ggplot2::aes(alpha = season), color = color_light, size = 1, position = ggplot2::position_nudge(y = 0.5)) +
  ggplot2::geom_point(data = subset(rfl_draft_hit_rates, season == var_season_last - 3), mapping = ggplot2::aes(size = total_picks), color = color_accent, position = ggplot2::position_nudge(y = 0.5)) +

  ggplot2::scale_size_continuous(range = c(1, 3.5)) +
  ggplot2::scale_alpha_continuous(range = c(0.2, 0.5), guide = "none") +
  ggplot2::scale_x_continuous(limits = c(-0.15, max(rfl_draft_hit_rates$hit_rate + 0.05)), labels = scales::percent) +
  ggplot2::scale_y_discrete(expand = c(0, 0, 0.03, 0)) +

  ggplot2::labs(
    title = paste0("Draft Hit Rates 2017-", var_season_last - 3),
    subtitle = "Ein Spieler wird als Hit kategorisiert, wenn er aktuell mehr als 0 WAR in seiner Karriere hat.\nJe dunkler die Punkte, desto länger ist der Draft her.",
    size = paste("Anzahl Picks", var_season_last - 3)
  ) +

  default_plot +
  ggplot2::theme(
    legend.position = "top",
    axis.text.y = ggplot2::element_text(hjust = 1, vjust = -0.85, margin = ggplot2::margin(r = -20, unit = "mm"), family = "accent")
  )

# output ----
output <- output %>%
  officer::add_slide(layout = "2 Bilder") %>%
  officer::ph_with(value = drafted_positions_plot, location = officer::ph_location_label("img-left")) %>%
  officer::ph_with(value = draft_hit_rates_plot, location = officer::ph_location_label("img-right"))
