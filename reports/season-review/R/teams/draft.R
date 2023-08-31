# data ----
rfl_draft_data_team <- rfl_draft_data %>%
  dplyr::filter(franchise == i)

rfl_draft_hit_rates_team <- rfl_draft_hit_rates %>%
  dplyr::filter(franchise_name == current_franchise_name)

# hit rates ----
draft_hit_rates_team_plot <- ggplot(rfl_draft_hit_rates_team, aes(x = season, y = hit_rate)) +
  ggplot2::geom_hline(yintercept = mean(rfl_draft_hit_rates$hit_rate), color = color_light, linetype = "dashed", linewidth = 0.3) +

  ggplot2::geom_col(data = subset(rfl_draft_hit_rates_team, season <= var_season_last - 3), fill = color_accent) +
  ggplot2::geom_col(data = subset(rfl_draft_hit_rates_team, season > var_season_last - 3), fill = NA, color = color_accent, linetype = "dashed") +

  ggplot2::scale_x_continuous(breaks = c(2017:var_season_last)) +
  ggplot2::scale_y_continuous(limits= c(0, 1), labels = scales::percent) +

  ggplot2::labs(
    title = "Draft Hit Rates",
    subtitle = "Abgebildet sind alle Rookie Drafts seit 2017.\nDie Drafts der letzten 3 Jahre können noch nicht final bewertet werden.",
    caption = "Als Hit wird ein Spieler definiert, der über 0 Karriere WAR hat."
  ) +

  default_plot


# output ----
output <- output %>%
  officer::add_slide(layout = "2 Bilder") %>%
  officer::ph_with(value = draft_hit_rates_team_plot, location = officer::ph_location_label("img-left"))
  #officer::ph_with(value = draft_hit_rates_plot, location = officer::ph_location_label("img-right"))
