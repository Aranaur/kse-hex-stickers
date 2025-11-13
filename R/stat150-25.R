# Load necessary libraries
library(here)

source(here('sticker_generator.R'))

folder_name <- "logo"
original_name <- basename(rstudioapi::getSourceEditorContext()$path)
file_name <- sub("\\.R$", ".png", original_name)

if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

# Start creating the sticker

font_add_google("Roboto Mono", "robotomono")
showtext_auto()

download.file(
  "https://drive.google.com/uc?export=download&id=1VD8bH6bFCrV_Teg9972Q8QnMV1zpW2Xb",
  destfile = here(folder_name, "penguin.png"),
  mode = "wb"
)

plot <- ggplot() +
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

sticker(
  plot,
  s_x = 1.0, s_y = 0.9,
  s_width = 2.02, s_height = 7,
  package = "STAT150",
  p_family = "robotomono",
  p_fontface = "bold",
  p_size = 52,
  p_color = "#4E413B",
  p_y = 1.6,
  h_fill = "#F3EAD3",
  h_color = "#4E413B",
  url = "R for Data Science | 2025",
  u_family = "robotomono",
  u_size = 15,
  u_color = "#786452",
  u_y = 0.075,
  u_x = 1,
  filename = here(folder_name, file_name),
  dpi = 1000
)

file_delete(here(folder_name, "penguin.png"))
