library(ggplot2)
library(dplyr)
library(hexSticker)
library(showtext)
library(ggimage)
library(fs)
library(here)

folder_name <- "logo"

theme_sticker <- function(size=1.25, ...) {
  center <- 1
  radius <- 1
  h <- radius
  w <- sqrt(3)/2 * radius
  m <- 1.05
  list(
    theme_transparent() +
      theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit = "lines"),
            strip.text = element_blank(),
            line = element_blank(),
            text = element_blank(),
            title = element_blank(), ...),
    coord_fixed(),
    scale_y_continuous(expand = c(0, 0), limits = c(center-h*m , center+h*m )),
    scale_x_continuous(expand = c(0, 0), limits = c(center-w*m , center+w*m ))
  )
}

font_add_google("Roboto Mono", "robotomono")
showtext_auto()

dir_create(here(folder_name))
download.file(
  "https://drive.google.com/uc?export=download&id=1VD8bH6bFCrV_Teg9972Q8QnMV1zpW2Xb",
  destfile = here(folder_name, "penguin.png"),
  mode = "wb"
)

color_palette <- c(
  "#D95F02",
  "#B32821",
  "#E69F00"
)

set.seed(73)
n_dots <- 40
bg_dots <- tibble(
  x = runif(n_dots, -1.5, 2.5),
  y = runif(n_dots, -0.5, 1.5),
  color = sample(color_palette, n_dots, replace = TRUE)
)

plot <- ggplot() +
  geom_point(data = bg_dots, aes(x = x, y = y, color = color), size = 1.7) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  coord_fixed(ratio = 1, xlim = c(-0.9, 1.9), ylim = c(-0.4, 1.5)) +
  geom_image(
    aes(
      x = mean(range(bg_dots$x)),
      y = mean(range(bg_dots$y)),
      image = "logo/penguin.png"),
    size = 0.95
  ) + 
  theme_transparent()

sticker_object <- sticker(
  plot,
  s_x = 1.0, s_y = 0.9,
  s_width = 2, s_height = 7,
  # Параметри заголовка "STAT150"
  package = "STAT150",
  p_family = "robotomono",
  p_fontface = "bold",
  p_size = 26,
  p_color = "#4E413B",
  p_y = 1.55,
  h_fill = "#F3EAD3",
  h_color = "#4E413B",
  url = "R for Data Science | 2025",
  u_family = "robotomono",
  u_size = 6,
  u_color = "#786452",
  u_y = 0.075,
  u_x = 1
)

final_plot <- sticker_object + theme_sticker()

ggsave(
  "logo/stat150.png", 
  plot = final_plot,
  width = 5.9,
  height = 6.8, 
  units = "cm",
  dpi = 300
)

file_delete(here(folder_name, "penguin.png"))