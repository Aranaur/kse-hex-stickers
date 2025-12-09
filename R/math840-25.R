# Load necessary libraries
library(here)

source(here('sticker_generator.R'))

folder_name <- "logo"
original_name <- basename(rstudioapi::getSourceEditorContext()$path)
file_name <- sub("\\.R$", ".png", original_name)

if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}


font_add_google("Roboto Mono", "robotomono")
font_add_google("Patrick Hand", "patrick") 
showtext_auto()

electricity_df <- read_csv("https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/energy_ua/energy_ua_2014_2021.csv") |> 
  pivot_longer(
    cols = -time,
    names_to = "source",
    values_to = "consumption_mwh"
  ) |> 
  mutate(
    month = month(time),
    year = year(time),
  ) |>
  group_by(year, month, source) |>
  summarise(consumption_mwh = sum(consumption_mwh, na.rm = TRUE)) |>
  ungroup()

plot_data <- electricity_df |> 
  filter(source == "TEC")

gradient_line_df <- data.frame(
  x = seq(min(acf_df$lag) - 0.5, max(acf_df$lag) + 0.5, length.out = 5000),
  y = 0
)

horst_palette <- c("#2E5090", "#009B9F", "#5FBC63", "#C4D600")

line_size <- 1.4 
offset_val <- 0.04

plot <- ggplot() +
  geom_line(
    data = gradient_line_df, 
    aes(x = x, y = y, color = x), 
    size = line_size, 
    alpha = 0.8,
    lineend = "round"
  ) +
  
  geom_segment(
    data = acf_df,
    aes(
      x = lag, xend = lag, 
      y = sign(acf) * offset_val, 
      yend = acf, 
      color = lag
    ), 
    size = 1.2, 
    alpha = 0.8,
    lineend = "round"
  ) +
  
  geom_point(
    data = acf_df,
    aes(x = lag, y = acf, color = lag), 
    size = 2.8
  ) +
  
  scale_color_gradientn(colors = horst_palette) +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")

sticker_object <- sticker(
  subplot = plot,
  s_x = 1.0, s_y = 0.85,
  s_width = 1.6, s_height = 1.2,

  package = "MATH840",
  p_family = "robotomono",
  p_fontface = "bold",
  p_size = 62,
  p_color = "#2E5090",
  p_y = 1.55,

  h_fill = "#FAFAFA",
  h_color = "#5FBC63",
  h_size = 1.5,

  url = "CSD@KSE | Time Series | 2025",
  u_family = "robotomono",
  u_size = 14,
  u_color = "#586E75",
  u_x = 0.99,
  u_y = 0.08,
  
  filename = here(folder_name, file_name),
  dpi = 1000
)
