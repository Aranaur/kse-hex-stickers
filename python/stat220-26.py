"""STAT220 (2026): two groups of an A/B test; the gap between the dashed means is the effect."""

import numpy as np

from hexsticker import sticker, sticker_path

BLUE, ORANGE, WHITE = "#447099", "#fb6107", "#FFFFFF"
FILL_A, FILL_B = "#7D9BBC", "#FDA06A"  # opaque tints: no alpha, so the overlap never turns muddy

D = 2.4                 # distance between the means (sd = 1)
MID = D / 2             # the curves cross here; each side takes the colour of the higher curve
x = np.union1d(np.linspace(-2.9, D + 2.9, 600), [MID])
a = np.exp(-0.5 * x**2) / np.sqrt(2 * np.pi)
b = np.exp(-0.5 * (x - D) ** 2) / np.sqrt(2 * np.pi)
top = 1 / np.sqrt(2 * np.pi)
left, right = x <= MID, x >= MID

# Sizes in points (see the unit notes in hexsticker.py).
LW_CURVE, LW_AXIS, LW_MEAN = 2.6, 1.6, 1.0


def draw(ax):
    ax.fill_between(x[left], 0, a[left], color=FILL_A, linewidth=0)
    ax.fill_between(x[right], 0, b[right], color=FILL_B, linewidth=0)
    ax.plot([x[0], MID], [0, 0], color=WHITE, lw=LW_AXIS, solid_capstyle="round")
    ax.plot([MID, x[-1]], [0, 0], color=ORANGE, lw=LW_AXIS, solid_capstyle="round")
    ax.plot(x, a, color=WHITE, lw=LW_CURVE, solid_capstyle="round")
    ax.plot(x, b, color=ORANGE, lw=LW_CURVE, solid_capstyle="round")
    for mu, c in ((0, WHITE), (D, ORANGE)):
        ax.plot([mu, mu], [0, top], color=c, lw=LW_MEAN, dashes=(2, 2))
    ax.set_ylim(-0.01, top * 1.08)


sticker(
    draw,
    s_x=1.0, s_y=0.92, s_width=1.24, s_height=0.86,
    package="STAT220", p_family="Roboto Mono", p_weight=700,
    p_size=16.94, p_color=WHITE, p_y=1.48,
    h_fill=BLUE, h_color=ORANGE, h_size=3.20,
    url="KSE | Applied Statistics | 2026", u_family="Roboto Mono",
    u_size=3.45, u_color="#DCE6F0", u_x=1.0, u_y=0.075,
    filename=sticker_path(), dpi=1000,
)
