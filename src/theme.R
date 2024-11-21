library(ggplot2)
library(ggtext)

theme_benedict <- function(base_size = 12,
                           dark_text = "#1A242F") {
  mid_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]

  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = mid_text, family = "IBM Plex Sans", lineheight = 1.1),
      plot.title = element_text(colour = dark_text, family = "IBM Plex Sans", face = "bold", size = rel(1.6), margin = margin(12, 0, 8, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1), face = "italic", padding = margin(0, 0, 30, 0)),
      axis.text.y = element_text(colour = light_text, size = rel(0.8)),
      axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
      axis.text.x = element_text(colour = mid_text, size = 10),
      plot.title.position = "plot",
      legend.position = "top",
      panel.grid = element_line(colour = "#F3F4F5"),
      plot.caption = element_text(size = rel(0.7), margin = margin(8, 0, 0, 0)),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

theme_maps <- function(base_size = 12,
                       dark_text = "#1A242F") {
  mid_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]

  theme_void(base_size = base_size) +
    theme(
      text = element_text(colour = mid_text, family = "IBM Plex Sans", lineheight = 1.1),
      plot.title = element_textbox_simple(colour = dark_text, family = "IBM Plex Sans", face = "bold", size = rel(1.6), margin = margin(12, 0, 12, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1), face = "italic", margin = margin(5, 0, 40, 0)),
      plot.title.position = "plot",
      legend.title = element_text(size = rel(0.9)),
      plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    )
}

COLORS <- c("#9a133d", "#b93961", "#d8527c", "#f28aaa", "#f9b4c9", "#f9e0e8", "#ffffff", "#eaf3ff", "#c5daf6", "#a1c2ed", "#6996e3", "#4060c8", "#1a318b")
