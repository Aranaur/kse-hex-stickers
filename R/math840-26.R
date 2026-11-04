source(here::here("R", "hexsticker.R"))

font_add_google("Roboto Mono", "robotomono")
showtext_auto()

# Two variants: the light one is the main logo (logo/math840-26.png),
# the dark one goes next to it as logo/math840-26-dark.png.

# A seasonal series, its forecast fan, and the seasonal naive benchmark it has to beat.
set.seed(840)
m <- 12; n <- 36; h <- 12
t <- 1:n
y <- 10 + 0.07 * t + 2 * sin(2 * pi * t / m) + 0.8 * sin(4 * pi * t / m) + rnorm(n, 0, 0.18)
th <- (n + 1):(n + h)
fc <- 10 + 0.07 * th + 2 * sin(2 * pi * th / m) + 0.8 * sin(4 * pi * th / m)
se <- 0.55 * sqrt(seq_len(h))
hist_df <- data.frame(x = c(t, n + 1), y = c(y, fc[1]))
fan <- data.frame(x = c(n, th), mid = c(y[n], fc),
                  lo95 = c(y[n], fc - 1.96 * se), hi95 = c(y[n], fc + 1.96 * se),
                  lo80 = c(y[n], fc - 1.28 * se), hi80 = c(y[n], fc + 1.28 * se))
snaive <- data.frame(x = th, y = y[n - m + ((seq_len(h) - 1) %% m) + 1])

horst <- c("#2E5090", "#009B9F", "#5FBC63", "#C4D600")

make_plot <- function(fan_col, bench_col) {
  ggplot() +
    geom_ribbon(data = fan, aes(x, ymin = lo95, ymax = hi95), fill = fan_col, alpha = 0.22) +
    geom_ribbon(data = fan, aes(x, ymin = lo80, ymax = hi80), fill = fan_col, alpha = 0.40) +
    geom_line(data = snaive, aes(x, y), colour = bench_col, linewidth = 0.55,
              linetype = "22", lineend = "round") +
    geom_line(data = hist_df, aes(x, y, colour = x), linewidth = 0.9, lineend = "round") +
    geom_line(data = fan, aes(x, mid), colour = fan_col, linewidth = 0.9, lineend = "round") +
    geom_point(data = data.frame(x = n, y = y[n]), aes(x, y), colour = horst[3], size = 1.8) +
    scale_colour_gradientn(colours = horst[1:3]) +
    theme_void() + theme_transparent() + theme(legend.position = "none")
}

variants <- list(
  light = list(fill = "#FAFAFA", border = "#5FBC63", text = "#2E5090", url = "#586E75",
               fan = "#C4D600", bench = "#9AA5AE"),
  dark  = list(fill = "#1F3561", border = "#C4D600", text = "#FAFAFA", url = "#C9D3E0",
               fan = "#C4D600", bench = "#8FA3BF")
)

for (v in names(variants)) {
  s <- variants[[v]]
  sticker(
    subplot = make_plot(s$fan, s$bench),
    s_x = 0.97, s_y = 0.92, s_width = 1.38, s_height = 1.0,
    package = "MATH840", p_family = "robotomono", p_fontface = "bold",
    p_size = 62, p_color = s$text, p_y = 1.55,
    h_fill = s$fill, h_color = s$border, h_size = 1.5,
    url = "CSD@KSE | Time Series | 2026", u_family = "robotomono",
    u_size = 14, u_color = s$url, u_x = 0.99, u_y = 0.08,
    filename = sticker_path(if (v == "light") "" else paste0("-", v)), dpi = 1000
  )
}
