"""MATH840 (2025): ACF of monthly thermal power (TEC) generation in Ukraine, 2014-2021."""

from collections import defaultdict

import numpy as np

from hexsticker import gradient, gradient_line, read_csv, sticker, sticker_path

DATA = "https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/energy_ua/energy_ua_2014_2021.csv"

monthly = defaultdict(float)
for row in read_csv(DATA):
    if row["TEC"] not in ("", "NA"):
        monthly[row["time"][:7]] += float(row["TEC"])  # "YYYY-MM"
x = np.array([monthly[k] for k in sorted(monthly)])


def acf(x, lag_max):
    d = x - x.mean()
    c0 = d @ d
    return np.array([d[: len(d) - k] @ d[k:] / c0 for k in range(lag_max + 1)])


acf_vals = acf(x, 13)
lags = np.arange(len(acf_vals))

horst = ["#2E5090", "#009B9F", "#5FBC63", "#C4D600"]
offset = 0.04
lo, hi = lags.min() - 0.5, lags.max() + 0.5  # one colour scale for all layers

# Sizes converted from the R script (see the unit notes in hexsticker.py).
LW_BASE, LW_STEM, POINT = 2.99, 2.56, 7.39


def draw(ax):
    base_x = np.linspace(lo, hi, 5000)
    gradient_line(ax, base_x, np.zeros_like(base_x), horst, LW_BASE, lo, hi, alpha=0.8)
    cols = gradient(horst, lags, lo, hi)
    for lag, a, c in zip(lags, acf_vals, cols):
        ax.plot([lag, lag], [np.sign(a) * offset, a], color=c, linewidth=LW_STEM,
                alpha=0.8, solid_capstyle="round")
    ax.scatter(lags, acf_vals, s=POINT ** 2, c=cols, linewidths=0, zorder=3)


sticker(
    draw,
    s_x=1.0, s_y=0.85, s_width=1.6, s_height=1.2,
    package="MATH840", p_family="Roboto Mono", p_weight=700,
    p_size=16.94, p_color="#2E5090", p_y=1.55,
    h_fill="#FAFAFA", h_color="#5FBC63", h_size=3.20,
    url="CSD@KSE | Time Series | 2025", u_family="Roboto Mono",
    u_size=3.82, u_color="#586E75", u_x=0.99, u_y=0.08,
    filename=sticker_path(), dpi=1000,
)
