plot_personnel <- function(data) {
  ggplot2::ggplot(data, aes(area = count, fill = as.factor(grouping_order), subgroup = grouping, subgroup2 = personnel)) +
    ggplot2::facet_wrap(~season, ncol = 3) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup2_border(color = color_bg, size = 2) +
    
    ggplot2::geom_text(data = subset(data, season != 2015), aes(label = season), x = 0.05, y = 0.05, vjust = 0, hjust = 0, family = "base", fontface = "bold", color = color_bg, size = 4.5) +
    ggplot2::geom_text(data = subset(data, xmax - xmin > 0.1), aes(label = ifelse(count_above_league_avg > 0, paste0("+", count_above_league_avg), count_above_league_avg), x = xmax - 0.05, y = ymax - 0.05), hjust = 1, vjust = 1, family = "base", fontface = "bold", color = color_bg, size = 4.5) +
    
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_shape_manual(values = c(17, 15, 19, 17, 1, 2, 3)) +
    ggplot2::guides(fill = "none", color = "none", shape = guide_legend()) +
    ggplot2::coord_fixed() +
    
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    
    ggplot2::theme_void() +
    default_plot +
    ggplot2::theme (
      legend.position = "none",
      panel.spacing = ggplot2::unit(2, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    )
}

## add points helper ----
plot_personnel_points <- function(data, pos) {
  geom_point <- ggplot2::geom_point(
    data = subset({{data}}, position == {{pos}} & is_to_big < 1),
    aes(x = xoffset, y = yoffset, shape = position),
    color = color_light,
    size = 1.3,
    alpha = 0.9
  )
}
