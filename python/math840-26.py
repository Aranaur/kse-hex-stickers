"""MATH840 (2026): a seasonal series, its forecast fan, and the seasonal naive benchmark it has to beat.

Two variants: the light one is the main logo (logo/math840-26.png),
the dark one goes next to it as logo/math840-26-dark.png.
"""

import numpy as np

from hexsticker import gradient_line, r_rnorm, sticker, sticker_path

m, n, h = 12, 36, 12
t = np.arange(1, n + 1)
y = 10 + 0.07 * t + 2 * np.sin(2 * np.pi * t / m) + 0.8 * np.sin(4 * np.pi * t / m) \
    + r_rnorm(n, 0, 0.18, seed=840)
th = np.arange(n + 1, n + h + 1)
fc = 10 + 0.07 * th + 2 * np.sin(2 * np.pi * th / m) + 0.8 * np.sin(4 * np.pi * th / m)
se = 0.55 * np.sqrt(np.arange(1, h + 1))

hist_x, hist_y = np.append(t, n + 1), np.append(y, fc[0])
fan_x = np.append(n, th)
mid = np.append(y[-1], fc)
lo95, hi95 = np.append(y[-1], fc - 1.96 * se), np.append(y[-1], fc + 1.96 * se)
lo80, hi80 = np.append(y[-1], fc - 1.28 * se), np.append(y[-1], fc + 1.28 * se)
snaive = y[n - m:][np.arange(h) % m]

horst = ["#2E5090", "#009B9F", "#5FBC63", "#C4D600"]

# Sizes converted from the R script (see the unit notes in hexsticker.py).
LW_MAIN, LW_BENCH, POINT = 1.92, 1.17, 5.26


def make_plot(fan_col, bench_col):
    def draw(ax):
        ax.fill_between(fan_x, lo95, hi95, color=fan_col, alpha=0.22, linewidth=0)
        ax.fill_between(fan_x, lo80, hi80, color=fan_col, alpha=0.40, linewidth=0)
        ax.plot(th, snaive, color=bench_col, linewidth=LW_BENCH, dashes=(2, 2),
                dash_capstyle="round", solid_capstyle="round")
        gradient_line(ax, hist_x, hist_y, horst[:3], LW_MAIN)
        ax.plot(fan_x, mid, color=fan_col, linewidth=LW_MAIN, solid_capstyle="round")
        ax.plot([n], [y[-1]], "o", color=horst[2], markersize=POINT, markeredgewidth=0)
    return draw


variants = {
    "light": dict(fill="#FAFAFA", border="#5FBC63", text="#2E5090", url="#586E75",
                  fan="#C4D600", bench="#9AA5AE"),
    "dark": dict(fill="#1F3561", border="#C4D600", text="#FAFAFA", url="#C9D3E0",
                 fan="#C4D600", bench="#8FA3BF"),
}

for v, s in variants.items():
    sticker(
        make_plot(s["fan"], s["bench"]),
        s_x=0.97, s_y=0.92, s_width=1.38, s_height=1.0,
        package="MATH840", p_family="Roboto Mono", p_weight=700,
        p_size=16.94, p_color=s["text"], p_y=1.55,
        h_fill=s["fill"], h_color=s["border"], h_size=3.20,
        url="CSD@KSE | Time Series | 2026", u_family="Roboto Mono",
        u_size=3.82, u_color=s["url"], u_x=0.99, u_y=0.08,
        filename=sticker_path("" if v == "light" else f"-{v}"), dpi=1000,
    )
