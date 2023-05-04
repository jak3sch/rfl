# postseason data
postseason <- purrr::map_df(2017:var_season_last, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/weeklyResults/rfl-results-{x}-postseason.csv")
  )
}) %>%
  dplyr::group_by(season, bowl, franchise_id) %>%
  dplyr::mutate(
    appearance = ifelse(week == min(week), 1, 0)
  ) %>%
  dplyr::group_by(bowl, franchise_id) %>%
  dplyr::arrange(season) %>%
  dplyr::mutate(
    total_appearances = sum(appearance),
    next_appearance = dplyr::lead(season)
  ) %>%
  dplyr::select(-appearance) %>%
  dplyr::left_join(
    latest_franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  )

superbowl <- postseason %>%
  dplyr::filter(
    bowl == "SB" & total_appearances > 1
  ) %>%
  dplyr::group_by(franchise_name, season) %>%
  dplyr::arrange(desc(po_finish)) %>%
  #dplyr::filter(row_number() == 1)
  dplyr::mutate(
    result_color = dplyr::case_when(
      po_finish == 1 ~ "1st",
      po_finish == 2 ~ "2nd",
      po_finish == 3 ~ "3rd",
      TRUE ~ "4th +"
    ),
    result_color = factor(result_color, levels = c("1st", "2nd", "3rd", "4th +"))
  ) %>%
  dplyr::select(season, franchise_name, total_appearances, po_finish, result_color, next_appearance) %>% 
  dplyr::filter(season != next_appearance | season == var_season_last) %>%
  dplyr::mutate(
    next_appearance = ifelse(season == var_season_last & next_appearance == season, NA, next_appearance),
    years = next_appearance - season,
    curvature = log(sin(years / 2)) - 0.2
  )

# plots ----
## helper ----
# function for a single geom_curve()
curve_geom <- function(franchise_name, xstart, xend, curvature, color) {
  ggplot2::geom_curve(
    # since I use a facet plot, I need the geom only visible in a specific panel
    data = data.frame(franchise_name = {{franchise_name}}),
    aes(x = {{xstart}}, xend = {{xend}}, color = {{color}}),
    y = 0.1, curvature = {{curvature}}, yend = 0.1, linewidth = 0.3)
}

# function to apply on the ggplot
add_curves <- function(franchise_name) {
  franchise_info <- superbowl %>%
    dplyr::filter(franchise_name == {{franchise_name}})
  
  # use helper function to create a geom_curve() for each entry in the franchise_info df
  mapply(
    curve_geom,
    franchise_name = {{franchise_name}},
    xstart = franchise_info$season,
    xend = franchise_info$next_appearance,
    curvature = franchise_info$curvature,
    color = franchise_info$result_color
  )
}

## superbowl ----
superbowl_plot <- ggplot2::ggplot(superbowl, aes(x = season, color = result_color, y = 0.1)) +
  ggplot2::facet_wrap(~franchise_name, scales = "free_y", ncol = 2, strip.position = "left", labeller = ggplot2::label_wrap_gen(width = 15))

### add curves ----
for (franchise in unique(super_bowl$franchise_name)) {
  superbowl_plot <- superbowl_plot +
    add_curves(franchise)
}

superbowl_plot <- super_bowl_plot +
  ggplot2::geom_point(aes(size = po_finish, color = result_color)) +
  
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::scale_size_continuous(range = c(1.5, 0.5), guide = "none") +
  ggplot2::scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0)) +
  
  ggplot2::labs(
    title = "Superbowl Teilnahmen",
    subtitle = "Die Grafik zeigt alle Teams mit mindestens zwei Super Bowl Teilnahmen.\nDie Länge der Bögen zeigt den Abstand zwischen den Teilnahmen, die Größe und Farbe der Punkte die Platzierung.",
    color = ""
  ) +
  
  default_plot +
  ggplot2::theme(
    legend.position = "top",
    strip.text.y.left = ggplot2::element_text(angle = 90),
    panel.spacing.x = ggplot2::unit(8, "mm"),
    panel.spacing.y = ggplot2::unit(5, "mm"),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  )
superbowl_plot

## other bowls ----
other_bowls_plot <- ggplot(data = subset(postseason, !(franchise_name %in% unique(super_bowl$franchise_name))), aes(x = season, y = franchise_name, size = po_finish, color = factor(bowl, levels = c("SB", "PB", "TB")))) +
  ggplot2::geom_rect(xmin = 2015, xmax = 2016.8, ymin = 0, ymax = 36, fill = color_bg, color = NA) +
  ggplot2::geom_point(position = ggplot2::position_nudge(y = 0.5)) +
  
  ggplot2::scale_color_manual(labels = c("Super Bowl", "Pro Bowl", "Toilet Bowl"), values = c("#ff9f43", "#2e86de", "#ee5253")) +
  ggplot2::scale_size_continuous(range = c(2, 0.5), guide = "none") +
  ggplot2::scale_x_continuous(limits = c(2015.8, var_season_last), expand = c(0, 0.05), breaks = 2017:var_season_last) +
  
  ggplot2::labs(
    title = "Superbowl Teilnahmen",
    subtitle = "Die Grafik zeigt alle Teams, die bislang nicht mindestens zwei Mal im Super Bowl Teilgenommen haben.\nDie Farbe der Punkte steht für den Bowl, an dem das Team teilgenommen hat, die Größe für die Platzierung (je größer, desto besser).",
    color = ""
  ) +
  
  default_plot +
  ggplot2::theme(
    legend.position = "top",
    axis.text.y = ggplot2::element_text(color = color_light, size = 14, hjust = 1, vjust = -1.3, margin = ggplot2::margin(r = -1.9, unit = "cm"), family = "accent"),
    panel.grid.major.x = ggplot2::element_blank()
  )
