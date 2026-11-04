source(here::here("R", "hexsticker.R"))

font_add_google("Roboto Mono", "robotomono")
showtext_auto()

# Two groups of an A/B test; the gap between the dashed means is the effect.
blue <- "#447099"; orange <- "#fb6107"; white <- "#FFFFFF"
fill_a <- "#7D9BBC"; fill_b <- "#FDA06A"  # opaque tints: no alpha, so the overlap never turns muddy

d <- 2.4        # distance between the means (sd = 1)
mid <- d / 2    # the curves cross here; each side takes the colour of the higher curve
x <- sort(unique(c(seq(-2.9, d + 2.9, length.out = 600), mid)))
curves <- data.frame(x = x, a = dnorm(x), b = dnorm(x, d))
top <- dnorm(0)
means <- data.frame(mu = c(0, d), colour = c(white, orange))

plot <- ggplot() +
  geom_ribbon(data = subset(curves, x <= mid), aes(x, ymin = 0, ymax = a), fill = fill_a) +
  geom_ribbon(data = subset(curves, x >= mid), aes(x, ymin = 0, ymax = b), fill = fill_b) +
  annotate("segment", x = min(x), xend = mid, y = 0, yend = 0, colour = white,
           linewidth = 0.75, lineend = "round") +
  annotate("segment", x = mid, xend = max(x), y = 0, yend = 0, colour = orange,
           linewidth = 0.75, lineend = "round") +
  geom_line(data = curves, aes(x, a), colour = white, linewidth = 1.22, lineend = "round") +
  geom_line(data = curves, aes(x, b), colour = orange, linewidth = 1.22, lineend = "round") +
  geom_segment(data = means, aes(x = mu, xend = mu, y = 0, yend = top, colour = colour),
               linewidth = 0.47, linetype = "22") +
  scale_colour_identity() +
  scale_y_continuous(limits = c(-0.01, top * 1.08), expand = c(0, 0)) +
  theme_void() + theme_transparent() + theme(legend.position = "none")

sticker(
  subplot = plot,
  s_x = 1.0, s_y = 0.92, s_width = 1.24, s_height = 0.86,
  package = "STAT220", p_family = "robotomono", p_fontface = "bold",
  p_size = 62, p_color = white, p_y = 1.48,
  h_fill = blue, h_color = orange, h_size = 1.5,
  url = "KSE | Applied Statistics | 2026", u_family = "robotomono",
  u_size = 12.6, u_color = "#DCE6F0", u_x = 1.0, u_y = 0.075,
  filename = sticker_path(), dpi = 1000
)
