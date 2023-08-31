percentile_key_order <- c("Win %", "All Win %", "Expected Wins", "Total Points", "Potential Points", "Points per Game", "Points Against", "Effizienz", "Luck", "Skill")

franchise_percentiles <- rfl_standings %>%
  dplyr::filter(franchise_id == i) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::mutate(
    dplyr::across(dplyr::ends_with("pctl_total"), mean, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    season == var_season_last | season == var_season_last - 1
  ) %>%
  tidyr::gather(key, pctl, c(ends_with("_season"), ends_with("_total"))) %>%
  dplyr::mutate(
    type = ifelse(grepl("_total", key), "total", "season"),
    key = sub("[^_]+$", "", key),
    key = case_when(
      key == "allplay_win_pctl_" ~ "All Win %",
      key == "efficiency_pctl_" ~ "Effizienz",
      key == "points_against_pctl_" ~ "Points Against",
      key == "points_for_pctl_" ~ "Total Points",
      key == "points_per_game_pctl_" ~ "Points per Game",
      key == "points_potential_pctl_" ~ "Potential Points",
      key == "sf_expw_pctl_" ~ "Expected Wins",
      key == "sf_luck_pctl_" ~ "Luck",
      key == "sf_skill_pctl_" ~ "Skill",
      key == "win_pctl_" ~ "Win %"
    )
  )

franchise_percentiles_additional_data <- franchise_percentiles %>%
  dplyr::filter(season == var_season_last) %>%
  dplyr::left_join(franchise_percentiles %>%  dplyr::filter(type == "season" & season == var_season_last - 1) %>% dplyr::select(key, pctl) %>% dplyr::rename(pctl_prev_season = pctl), by = "key") %>%
  dplyr::filter(type == "season") %>%
  dplyr::left_join(franchise_percentiles %>% dplyr::filter(type == "total" & season == var_season_last) %>%  dplyr::select(key, pctl) %>% dplyr::rename(pctl_total = pctl), by = "key") %>%
  dplyr::mutate(key_f = factor(key, percentile_key_order)) %>%
  dplyr::arrange(key_f) %>%
  dplyr::mutate(key_rank = dplyr::row_number()) %>%
  dplyr::mutate(
    spape = 21, shape = ifelse(pctl > pctl_prev_season, "arrow_up", "arrow_down"),

    # find empty bar für label
    empty_bar = case_when(
      pctl_total < pctl & pctl_total < pctl_prev_season ~ 1, # pctl der letzten beiden jahre sind beide größer als total
      pctl_total < pctl & pctl_total > pctl_prev_season ~ 2, # total pctl ist nur größer als pctl prev season
      pctl_total == max(pctl_total) ~ 3, # der höchste pctl balken
      TRUE ~ 0
    ),

    empty_bar = case_when(
      key_rank > 1 & dplyr::lag(empty_bar) == 0 ~ 0,
      key_rank < max(key_rank) & dplyr::lead(empty_bar) == 0 ~ 0,
      TRUE ~ empty_bar
    )
  ) %>%
  dplyr::select(season, key, key_rank, type, pctl, pctl_prev_season, pctl_total, shape, empty_bar)

# labels
franchise_percentiles_labels <- franchise_percentiles_additional_data %>%
  dplyr::filter(empty_bar > 0) %>%
  dplyr::filter(pctl_total == max(pctl_total)) %>%
  dplyr::mutate(
    label_type = "bar"
  ) %>%

  # points
  rbind(
    franchise_percentiles_additional_data %>%
      dplyr::mutate(diff = pctl - pctl_prev_season) %>%
      dplyr::arrange(desc(diff)) %>%
      utils::head(1) %>%
      dplyr::select(-diff) %>%
      dplyr::mutate(
        label_type = "point"
      )
  ) %>%

  # bar label
  dplyr::mutate(
    label = dplyr::case_when(
      label_type == "bar" ~ paste("ø der Perzentile\nvon", var_season_first, "-", var_season_last),
      label_type == "point" ~ paste0("Perzentile der Saison ", var_season_last, ".\nRichtung markiert Veränderung zum Vorjahr (Quadrat).")
    ),

    label_align = dplyr::case_when(
      label_type == "bar" & key_rank <= 7 ~ "left",
      label_type == "point" & key_rank <= 5 ~ "left",
      TRUE ~ "right"
    )
  )

franchise_percentiles_plot <- ggplot2::ggplot(
  data = subset(franchise_percentiles, type == "total" & season == var_season_last),
  aes(x = factor(key, percentile_key_order), y = pctl * 100, fill = pctl)) +

  ggplot2::geom_col(fill = color_muted) +

  # verbindungslinie zwischen den season stats
  ggplot2::geom_segment(franchise_percentiles_additional_data, mapping = aes(xend = key, yend = pctl_prev_season * 100), linewidth = 0.3, color = color_light) +

  # total pctl
  geom_text(aes(label = round(pctl * 100, digits = 0)), nudge_y = 5, size = 16, color = color_light, family = "accent") +

  # season stats
  ggplot2::geom_point(data = subset(franchise_percentiles, type == "season" & season == var_season_last - 1), size = 2, shape = 22, stroke = NA) +
  ggplot2::geom_point(
    data = subset(franchise_percentiles_additional_data, type == "season" & season == var_season_last),
    aes(shape = shape),
    color = color_bg, size = 5, stroke = 0.3
  ) +

  ggplot2::scale_shape_manual(values = c("arrow_up" = 24, "arrow_down" = 25)) +
  ggplot2::scale_fill_gradient2(low = color_warning, high = color_accent, midpoint = 0.5) +
  ggplot2::scale_x_discrete(labels = function(x){sub("\\s", "\n", x)}) +
  ggplot2::scale_y_continuous(limits = c(0,115), expand = c(0,0), breaks = c(25, 50, 75, 100), labels = scales::label_percent(scale = 1)) +

  default_plot +

  ggplot2::labs(
    title = "Perzentile",
    subtitle = paste0("Die Balken zeigen die Gesamtperzentile seit ", var_season_first, ".\nDie Dreiecke zeigen die Perzentile der Saison ", var_season_last, " und die Veränderung zur Vorsaison (Quadrate)."),
  ) +

  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 26),
    axis.text.x = ggplot2::element_text(vjust = 1, lineheight = 0.35, family = "accent", margin = ggplot2::margin(t = 5))
  )

output <- output %>%
  officer::add_slide(layout = "Bild") %>%
  officer::ph_with(value = franchise_percentiles_plot, location = officer::ph_location_label("img"))

rm(percentile_key_order, franchise_percentiles, franchise_percentiles_additional_data, franchise_percentiles_labels, franchise_percentiles_plot)
