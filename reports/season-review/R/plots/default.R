# Fonts ----
sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

# Colors ----
color_bg <- "#222f3e"
color_light <- "#c8d6e5"
color_accent <- "#feca57"
colors <- c("#2e86de", "#ee5253", "#10ac84",  "#ff9f43", "#f368e0", "#0abde3", "#feca57", "#1dd1a1", "#8395a7", "#5f27cd")
colors_contrast <- c("#ff9ff3", "#5f27cd", "#feca57", "#1dd1a1", "#54a0ff", "#c8d6e5", "#10ac84", "#ff9f43", "#00d2d3", "#ff6b6b")

# Plots ----
default_plot <- list(
    ggplot2::theme (
      legend.position = "none",
      panel.spacing = ggplot2::unit(3, "mm"),
      strip.background = ggplot2::element_blank(),
      
      plot.title = ggplot2::element_text(
        size = 61,
        hjust = 0,
        color = color_accent,
        lineheight = 0.3,
        family = "accent",
        margin = ggplot2::margin(b = 2, unit = "mm")
      ),
      plot.subtitle = ggplot2::element_text(
        size = 31,
        family = "base",
        face = "bold",
        hjust = 0,
        lineheight = 0.4,
        color = color_light,
        margin = ggplot2::margin(b = 6, unit = "mm")
      )
    )
)
