# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# RESAS THEME =====================================================================================
# /////////////////////////////////////////////////////////////////////////////////////////////////

# This script sets the RESAS theme as the default ggplot2 theme and also it as an object in your
# workspace for ad-hoc use in your plots.

# Load ggplot2:
library(ggplot2)

# Set font:
windowsFonts(sans = windowsFont("Arial"))

# PLOT THEME ======================================================================================

# Create theme:
theme_resas <- theme_bw(base_size = 12) +
  theme(
    text = element_text(face = "plain", size = 12, colour = "black", lineheight = 1.2, margin = margin(t = 1, b = 1)),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(face = "plain", size = 12, colour = "black"),
    legend.text = element_text(face = "plain", size = 12, colour = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_line(colour = "grey90"),
    axis.ticks.x = element_blank(),
    axis.text = element_text(family = "sans", size = 12, colour = "black"),
    axis.title = element_text(family = "sans", size = 12, colour = "black"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    axis.line = element_blank(),
    plot.title = element_text(family = "sans", size = 16, face = "bold", colour = "black"),
    plot.subtitle = element_text(family = "sans", size = 14, colour = "grey10"),
    plot.caption = element_text(family = "sans", size = 12, colour = "grey10"),
    strip.background = element_blank(),
    strip.text = element_text(family = "sans", face = "bold", size = 12, colour = "black")
  )

# Update theme used by default:
theme_set(theme_resas)

# Double resolution version:

theme_resas_retina <- theme_bw(base_size = 24) +
  theme(
    text = element_text(face = "plain", size = 24, colour = "black", lineheight = 1.2, margin = margin(t = 1, b = 1)),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(face = "plain", size = 24, colour = "black"),
    legend.text = element_text(face = "plain", size = 24, colour = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_line(colour = "grey90"),
    axis.ticks.x = element_blank(),
    axis.text = element_text(family = "sans", size = 24, colour = "black"),
    axis.title = element_text(family = "sans", size = 24, colour = "black"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    axis.line = element_blank(),
    plot.title = element_text(family = "sans", size = 32, face = "bold", colour = "black"),
    plot.subtitle = element_text(family = "sans", size = 28, colour = "grey10"),
    plot.caption = element_text(family = "sans", size = 24, colour = "grey10"),
    strip.background = element_blank(),
    strip.text = element_text(family = "sans", face = "bold", size = 24, colour = "black")
  )

# PLOT EXAMPLES ===================================================================================

# These are used in the ggplot2 guidance document.

run_example = FALSE

while(run_example == TRUE){
  
  plotColours <- list(
    "green" = "#2C8B53",
    "dark_green" = "#194025",
    "light_green" = "#3ED581"
  )
  
  # Plot with title and subtitle:
  
  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point(aes(colour = Species), size = 2) +
    scale_colour_manual(values = c(
      setosa = plotColours$green,
      versicolor = plotColours$dark_green,
      virginica = plotColours$light_green
    )) +
    labs(
      title = "Title here",
      subtitle = "Subtitle here",
      caption = "caption here"
    )
  
  ggsave(last_plot(), filename = "test_plot_with_title.png", width = 159, height = 100, units = "mm", dpi = "retina", bg = "white")
  
  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point(aes(colour = Species), size = 2) +
    scale_colour_manual(values = c(
      setosa = plotColours$green,
      versicolor = plotColours$dark_green,
      virginica = plotColours$light_green
    )) +
    labs(
      title = "Title here",
      subtitle = "Subtitle here",
      caption = "caption here"
    ) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  ggsave(last_plot(), filename = "test_plot_without_title.png", width = 159, height = 100, units = "mm", dpi = "retina", bg = "white")
  
  library(dplyr)
  
  iris_avg <- iris %>%
    group_by(Species) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    ungroup()
  
  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point(aes(colour = Species), size = 2) +
    geom_text(data = iris_avg, aes(label = Species, colour = Species, x = c(4.5, 5.5, 7.5), y = c(4, 2, 3.5)), fontface = "bold") +
    scale_colour_manual(values = c(
      setosa = plotColours$green,
      versicolor = plotColours$dark_green,
      virginica = plotColours$light_green
    )) +
    labs(
      title = "Title here",
      subtitle = "Subtitle here",
      caption = "caption here"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  ggsave(last_plot(), filename = "test_plot_with_original_labels.png", width = 159, height = 100, units = "mm", dpi = "retina", bg = "white")
  
  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point(aes(colour = Species), size = 2) +
    geom_text(data = iris_avg, aes(label = Species, colour = Species, x = c(4.5, 5.5, 7.5), y = c(4, 2, 3.5)), fontface = "bold", size = 12 / .pt) +
    scale_colour_manual(values = c(
      setosa = plotColours$green,
      versicolor = plotColours$dark_green,
      virginica = plotColours$light_green
    )) +
    labs(
      title = "Title here",
      subtitle = "Subtitle here",
      caption = "caption here"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  ggsave(last_plot(), filename = "test_plot_with_12pt_labels.png", width = 159, height = 100, units = "mm", dpi = "retina", bg = "white")
  
  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point(aes(colour = Species), size = 4) +
    geom_text(data = iris_avg, aes(label = Species, colour = Species, x = c(4.5, 5.5, 7.5), y = c(4, 2, 3.5)), fontface = "bold", size = 24 / .pt) +
    scale_colour_manual(values = c(
      setosa = plotColours$green,
      versicolor = plotColours$dark_green,
      virginica = plotColours$light_green
    )) +
    labs(
      title = "Title here",
      subtitle = "Subtitle here",
      caption = "caption here"
    ) +
    theme_resas_retina +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  ggsave(last_plot(), filename = "test_plot_with_12pt_labels_double_resolution.png", width = 318, height = 200, units = "mm", dpi = "retina", bg = "white")
  
}