plotSource <- "Chart: @JakobEschler"

# sehr kleine texte in plots
plot.geom_text_xsmall_size <- 2
plot.geom_text_xsmall_family <- var.fontTextBold

# kleine texte in plots
plot.geom_text_small_size <- 2.8
plot.geom_text_small_family <- var.fontText

# highlight texte in plots
plot.geom_text_large_size <- 4
plot.geom_text_large_family <- var.fontAccent
plot.geom_text_large_color <- var.colorYellow

# shadow texte in plots
plot.geom_text_shadow_size <- 3
plot.geom_text_shadow_family <- var.fontTextBold
plot.geom_text_shadow_color <- var.colorLightblue

# plot theme
plot.theme_text_shadow_size <- 8
plot.theme_text_shadow_family <- var.fontTextBold
plot.theme_text_shadow_color <- var.colorLightblue

plot.line_width = 0.1
plot.line_width_m = plot.line_width * 2
plot.line_width_l = 0.8

plotDefaults <- list(
  labs(
    x = NULL,
    y = NULL
  ),
  theme(
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15),
    plot.background = element_rect(fill = var.colorBlue),

    axis.text = element_text(size = plot.theme_text_shadow_size, color = plot.theme_text_shadow_color, family = plot.theme_text_shadow_family),

    panel.background = element_blank(),
    panel.grid.major = element_line(color = var.colorLightblue, size = plot.line_width),

    text = element_text(color = var.colorBlue, family = var.fontText),

    strip.background = element_rect(fill = NA),
    strip.text = element_text(size = plot.theme_text_shadow_size, color = plot.geom_text_shadow_color, family = plot.theme_text_shadow_family),

    plot.title = element_text(size = 12, hjust = 0, family = var.fontTextBold, color = var.colorAccent, lineheight = 1.1, margin=margin(0,0,10,0)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12, hjust = 0, family = var.fontText, color = var.colorAccent, lineheight = 1, margin=margin(0,0,20,0)),

    #axis.title = element_text(size = 10, color = var.colorAccent, family = var.fontText),
    #axis.title.x = element_text(vjust = -1),
    #axis.title.y = element_text(vjust = 2.5),

    #axis.line = element_line(color = var.colorAccent, linewidth = 0.5),
    #axis.ticks = element_line(color = var.colorAccent, linewidth = 0.5),



    legend.background = element_blank(),
    #legend.title = element_text(size = 10, color = var.colorAccent, family = var.fontTextBold),
    legend.key = element_blank(),
    #legend.key.size = unit(4, "pt"),
    legend.text = element_text(size = plot.theme_text_shadow_size, color = plot.theme_text_shadow_color, family = plot.theme_text_shadow_family),

    #panel.grid.minor = element_line(color = var.colorBlue, linewidth = 0.25)
  )
)

plotDefaultsMinimal <- list(
  plotDefaults,
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_blank()
  )
)

# Histogram ----
plotHistogram <- list(
  plotDefaultsMinimal,
  labs(
    y = "Häufigkeit",
    fill = ""
  ),
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.key.size = unit(12, "pt"),
  ),
  scale_y_continuous(expand = c(0,0))
)

# heat map win percentage ----
plotHeatmap <- list(
  plotDefaults,
  geom_tile(),
  scale_fill_gradient(low = var.colorBlue, high = var.colorYellow),
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(12, "pt"),
  )
)
