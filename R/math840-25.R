library(ggplot2)
library(dplyr)
library(hexSticker)
library(showtext)
library(ggimage)
library(readr)
library(tidyr)
library(lubridate)

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

electricity_df <- read_csv("https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/energy_ua/energy_ua_2014_2021.csv") |> 
  pivot_longer(
    cols = -time,
    names_to = "source",
    values_to = "consumption_mwh"
  ) |> 
  # quarterly consumption
  mutate(
    month = month(time),
    year = year(time),
  ) |>
  group_by(year, month, source) |>
  summarise(consumption_mwh = sum(consumption_mwh, na.rm = TRUE)) |>
  ungroup()

plot_data <- electricity_df |> 
  filter(source == "TEC")

plot <- acf(plot_data$consumption_mwh, plot = FALSE, lag.max = 15) |> 
  with(data.frame(lag = lag, acf = acf)) |> 
  ggplot(aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = "#002B36", size = 1) +
  geom_hline(yintercept = 0, color = "#002B36", size = 1) +
  theme_transparent() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

sticker_object <- sticker(
  subplot = plot,
  s_x = 1.0, s_y = 0.8,
  s_width = 1.5, s_height = 1.4,
  package = "MATH840",
  p_family = "robotomono",
  p_fontface = "bold",
  p_size = 28,
  p_color = "#002B36",
  p_y = 1.5,
  h_fill = "#E8EEF2",
  h_color = "#586E75",
  url = "KSE@CSD | Time Series | 2025",
  u_family = "robotomono",
  u_size = 5.5,
  u_color = "#586E75",
  u_x = 0.99,
  u_y = 0.07,
)

final_plot <- sticker_object + theme_sticker()

ggsave(
  paste0(folder_name, "/math840-25.png"),
  plot = final_plot,
  width = 5.9,
  height = 6.8, 
  units = "cm",
  dpi = 300
)
