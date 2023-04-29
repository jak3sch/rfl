# Fonts ----
sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

# Colors ----
color_bg <- "#222f3e"
color_light <- "#c8d6e5"
color_accent <- "#feca57"
colors <- c("#2e86de", "#ee5253", "#10ac84",  "#ff9f43")

# Plots ----
default_plot <- list(
    ggplot2::theme (
      legend.position = "none",
      panel.spacing = ggplot2::unit(3, "mm"),
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = 22,
        color = color_accent,
        family = "accent"
      )
    )
)