"""STAT150 (2025): R for Data Science penguin."""

import tempfile
from pathlib import Path

import matplotlib.image as mpimg

from hexsticker import download, sticker, sticker_path

PENGUIN = "https://drive.google.com/uc?export=download&id=1VD8bH6bFCrV_Teg9972Q8QnMV1zpW2Xb"

# Same frame as the R version: coord_fixed(xlim = c(-0.9, 1.9), ylim = c(-0.4, 1.5))
# plus ggplot2's default 5 % expansion; the image takes 95 % of the panel height.
XLIM, YLIM = (-0.9, 1.9), (-0.4, 1.5)
CX, CY = 0.52, 0.57


def expand(lim, mult=0.05):
    d = (lim[1] - lim[0]) * mult
    return lim[0] - d, lim[1] + d


with tempfile.TemporaryDirectory() as tmp:
    img = mpimg.imread(download(PENGUIN, Path(tmp) / "penguin.png"))


def draw(ax):
    xlim, ylim = expand(XLIM), expand(YLIM)
    side = 0.95 * (ylim[1] - ylim[0])
    ax.imshow(img, extent=(CX - side / 2, CX + side / 2, CY - side / 2, CY + side / 2),
              interpolation="antialiased")
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    ax.set_aspect("equal")


sticker(
    draw,
    s_x=1.0, s_y=0.9, s_width=2.02, s_height=7,
    package="STAT150", p_family="Roboto Mono", p_weight=700,
    p_size=14.20, p_color="#4E413B", p_y=1.6,
    h_fill="#F3EAD3", h_color="#4E413B", h_size=2.56,
    url="R for Data Science | 2025", u_family="Roboto Mono",
    u_size=4.10, u_color="#786452", u_y=0.075, u_x=1,
    filename=sticker_path(), dpi=1000,
)
