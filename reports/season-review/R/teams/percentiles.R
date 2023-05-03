percentile_key_order <- c("Win %", "All Win %", "Expected Wins", "Total Points", "Potential Points", "Points per Game", "Points Against", "Effizienz", "Luck", "Skill")

franchise_percentiles <- standings %>%
  filter(
    franchise_id == i
  ) %>%
  group_by(franchise_id) %>%
  mutate(
    across(ends_with("pctl_total"), mean)
  ) %>%
  ungroup() %>%
  filter(
    season == var.lastSeason | season == var.lastSeason - 1
  ) %>%
  gather(key, pctl, c(ends_with("_season"), ends_with("_total"))) %>%
  mutate(
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
  dplyr::filter(season == var.lastSeason) %>%
  dplyr::left_join(franchise_percentiles %>%  dplyr::filter(type == "season" & season == var.lastSeason - 1) %>% dplyr::select(key, pctl) %>% dplyr::rename(pctl_prev_season = pctl), by = "key") %>%
  dplyr::filter(type == "season") %>%
  dplyr::left_join(franchise_percentiles %>% dplyr::filter(type == "total" & season == var.lastSeason) %>%  dplyr::select(key, pctl) %>% dplyr::rename(pctl_total = pctl), by = "key") %>%
  dplyr::mutate(key_f = factor(key, key_order)) %>%
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
      label_type == "bar" ~ paste("ø der Perzentile\nvon", var.firstSeason, "-", var.lastSeason),
      label_type == "point" ~ paste0("Perzentile der Saison ", var.lastSeason, ".\nRichtung markiert Veränderung zum Vorjahr (Quadrat).")
    ),

    label_align = dplyr::case_when(
      label_type == "bar" & key_rank <= 7 ~ "left",
      label_type == "point" & key_rank <= 5 ~ "left",
      TRUE ~ "right"
    )
  )

chart <- ggplot(data = subset(franchise_percentiles, type == "total" & season == var.lastSeason), aes(x = factor(key, key_order), y = pctl * 100, fill = pctl)) +
  plotDefaultsMinimal +

  geom_col(fill = var.colorAccent, alpha = 0.1) +

  # verbindungslinie zwischen den season stats
  geom_segment(franchise_percentiles_additional_data, mapping = aes(xend = key, yend = pctl_prev_season * 100), size = plot.line_width_l, color = var.colorLightblue) +

  # total pctl
  plot_geom_large_text(color = var.colorAccent, aes(label = round(pctl * 100, digits = 0)), nudge_y = 5) +

  # season stats
  geom_point(data = subset(franchise_percentiles, type == "season" & season == var.lastSeason - 1), size = 2, shape = 22, stroke = NA, position = position_nudge(x = 0.01)) +
  geom_point(
    data = subset(franchise_percentiles_additional_data, type == "season" & season == var.lastSeason),
    aes(shape = shape),
    color = var.colorBlue, size = 5, stroke = plot.line_width_l
  ) +

  # labels
  geom_curve(
    data = subset(franchise_percentiles_labels, label_type == "bar"),
    aes(x = key_rank + 0.1, xend = key_rank + 0.65, y = pctl_total * 10, yend = pctl_total * 20),
    curvature = -0.5, color = var.colorAccent, size = plot.line_width_m) +

  plot_geom_xsmall_text(
    data = subset(franchise_percentiles_labels, label_type == "bar"),
    aes(label = label, hjust = ifelse(label_align == "left", 0, 1), x = ifelse(label_align == "left", key_rank + 0.8, key_rank - 0.8), y = pctl_total * 10),
    vjust = 0, color = var.colorAccent) +

  geom_curve(
    data = subset(franchise_percentiles_labels, label_type == "point"),
    aes(xend = key_rank),
    yend = 105, curvature = -0.5, color = var.colorAccent, size = plot.line_width_m) +

  plot_geom_xsmall_text(
    data = subset(franchise_percentiles_labels, label_type == "point"),
    aes(label = label, x = ifelse(label_align == "left", 1, max(franchise_percentiles_additional_data$key_rank)), hjust = ifelse(label_align == "left", 0, 1)),
    y = 108, vjust = 0, color = var.colorAccent) +

  # theme

  labs(
    title = toupper(paste0(franchise$franchise_name, ": Perzentile ", var.lastSeason))
  ) +

  scale_shape_manual(values = c("arrow_up" = 24, "arrow_down" = 25)) +
  scale_fill_gradient2(low = var.colorRed, high = var.colorYellow, midpoint = 0.5) +
  scale_x_discrete(labels = function(x){sub("\\s", "\n", x)}) +
  scale_y_continuous(limits = c(0,115), expand = c(0,0), breaks = c(25, 50, 75, 100)) +

  theme(
    panel.grid.major.y = element_line(size = plot.line_width, color = var.colorLightblue),
    axis.text = element_text(size = plot.theme_text_shadow_size, color = plot.theme_text_shadow_color, family = plot.theme_text_shadow_family)
    #legend.key.size = unit(12, "pt"),
    #plot.margin = margin(15, 15, 5, 15),
  )
