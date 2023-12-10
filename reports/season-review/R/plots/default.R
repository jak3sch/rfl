# Plots ----
default_plot <- list(
  ggplot2::theme_void(),
  ggplot2::theme (
    plot.background = ggplot2::element_rect(fill = c_background, color = NA),
    plot.margin = ggplot2::margin(c(5, 5, 5, 5), unit = "mm"),
    plot.title = ggplot2::element_text(
      size = 61,
      hjust = 0.5,
      color = c_accent,
      lineheight = 0.3,
      family = "accent",
      margin = ggplot2::margin(b = 4, unit = "mm")
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      size = 28,
      family = "base",
      hjust = 0.5,
      lineheight = 0.4,
      color = c_light,
      margin = ggplot2::margin(b = 6, unit = "mm")
    ),

    legend.position = "none",
    legend.text = ggplot2::element_text(color = c_light, size = 20),
    legend.title = ggplot2::element_text(color = c_light, size = 20),
    legend.key.size = unit(3, "mm"),

    panel.spacing = ggplot2::unit(3, "mm"),
    panel.grid.major = ggplot2::element_line(color = color_muted, linewidth = 0.1),

    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(color = c_light, family = "accent", size = 14, lineheight = 0.35),

    axis.text = ggplot2::element_text(color = c_light, family = "base", size = 16),
    axis.text.x = ggplot2::element_text(vjust = -4),
    plot.caption = ggplot2::element_text(color = c_light, size = 14, margin = ggplot2::margin(t = 30)),
  )
)

default_plot_minimal <- list(
  default_plot,
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank()
  )
)

# geoms
plot_geom_large_text <- function(color = c_light, ...) {
  geom_text(color = color, size = 6, family = "accent", ...)
}

plot_geom_xsmall_text <- function(...) {
  geom_text(size = 2, family = "accent", ...)
}

