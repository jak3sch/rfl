# Fonts ----
sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

# Colors ----
color_bg <- "#222f3e"
color_light <- "#c8d6e5"
color_accent <- "#feca57"
color_muted <- "#425B78"
colors <- c("#2e86de", "#ee5253", "#10ac84",  "#ff9f43", "#f368e0", "#0abde3", "#feca57", "#1dd1a1", "#8395a7", "#5f27cd")
colors_contrast <- c("#ff9ff3", "#5f27cd", "#feca57", "#1dd1a1", "#54a0ff", "#c8d6e5", "#10ac84", "#ff9f43", "#00d2d3", "#ff6b6b")

# Plots ----
default_plot <- list(
  ggplot2::theme_void(),
  ggplot2::theme (
    plot.background = ggplot2::element_rect(fill = color_bg, color = NA),
    plot.margin = ggplot2::margin(c(5, 5, 5, 5), unit = "mm"),
    plot.title = ggplot2::element_text(
      size = 61,
      hjust = 0.5,
      color = color_accent,
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
      color = color_light,
      margin = ggplot2::margin(b = 6, unit = "mm")
    ),

    legend.position = "none",
    legend.text = ggplot2::element_text(color = color_light, size = 14),

    panel.spacing = ggplot2::unit(3, "mm"),
    panel.grid.major = ggplot2::element_line(color = color_muted, linewidth = 0.1),

    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(color = color_light, family = "accent", size = 14, lineheight = 0.35),

    axis.text = ggplot2::element_text(color = color_light, family = "base", size = 16),
    axis.text.x = ggplot2::element_text(vjust = -4)
  )
)
