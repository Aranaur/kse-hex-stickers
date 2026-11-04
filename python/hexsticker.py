"""Hexagon sticker generator: a Python port of R/hexsticker.R.

Produces the same geometry as the R version (hexb.in standard, 43.9 x 50.8 mm,
pointy top) with matplotlib as the only dependency. All sizes are in points.

Unit notes for porting from the R scripts (ggplot2 + showtext, dpi = 1000):
  * line width:  pt = linewidth * 72.27 / 25.4 * 0.75
  * point size:  diameter pt = 0.75 * (size * 72.27 / 25.4 + 1.89)   (shape 19 + its stroke)
  * text size:   pt = size * 72.27 / 25.4 * 96 / 1000   (showtext renders at 96 dpi)
"""

import csv
import io
import re
import sys
import urllib.request
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.collections import LineCollection  # noqa: E402
from matplotlib.colors import to_rgb  # noqa: E402
from matplotlib.font_manager import FontProperties  # noqa: E402
from matplotlib.patches import Polygon  # noqa: E402
from matplotlib.textpath import TextPath  # noqa: E402
from matplotlib.transforms import offset_copy  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
CACHE = ROOT / ".cache"

WIDTH_MM, HEIGHT_MM = 43.9, 50.8
_HALF_W = np.sqrt(3) / 2
HEX_X = 1 + np.array([-_HALF_W, -_HALF_W, 0, _HALF_W, _HALF_W, 0])
HEX_Y = 1 + np.array([0.5, -0.5, -1, -0.5, 0.5, 1])


# --- files ----------------------------------------------------------------

def sticker_path(suffix=""):
    """logo/python/<script name><suffix>.png; an optional first CLI argument overrides the folder."""
    folder = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / "logo" / "python"
    folder.mkdir(parents=True, exist_ok=True)
    return folder / f"{Path(sys.argv[0]).stem}{suffix}.png"


def download(url, dest):
    req = urllib.request.Request(url, headers={"User-Agent": "kse-hex-stickers"})
    with urllib.request.urlopen(req) as r:
        Path(dest).write_bytes(r.read())
    return Path(dest)


def read_csv(source):
    """Rows of a CSV file (local path or URL) as dicts."""
    if re.match(r"https?://", str(source)):
        with urllib.request.urlopen(source) as r:
            text = r.read().decode("utf-8")
    else:
        text = Path(source).read_text(encoding="utf-8")
    return list(csv.DictReader(io.StringIO(text)))


def google_font(family, weight=400):
    """Path to a TTF from Google Fonts, downloaded once into .cache/fonts."""
    path = CACHE / "fonts" / f"{family.replace(' ', '')}-{weight}.ttf"
    if not path.exists():
        css_url = f"https://fonts.googleapis.com/css2?family={family.replace(' ', '+')}:wght@{weight}"
        with urllib.request.urlopen(css_url) as r:
            css = r.read().decode()
        ttf = re.search(r"url\((https://[^)]+\.ttf)\)", css)
        if not ttf:
            raise RuntimeError(f"No TTF for {family} {weight} in Google Fonts response")
        path.parent.mkdir(parents=True, exist_ok=True)
        download(ttf.group(1), path)
    return path


# --- R-compatible random numbers -----------------------------------------

def r_rnorm(n, mean=0.0, sd=1.0, seed=None, _state={}):
    """Same numbers as R's `set.seed(seed); rnorm(n, mean, sd)` (Mersenne-Twister + Inversion).

    Pass `seed` to reset the stream, as set.seed() does; omit it to continue.
    """
    from statistics import NormalDist

    if seed is not None:
        s = seed & 0xFFFFFFFF
        for _ in range(50):  # R's initial scrambling
            s = (69069 * s + 1) & 0xFFFFFFFF
        key = np.empty(625, dtype=np.uint32)
        for j in range(625):
            s = (69069 * s + 1) & 0xFFFFFFFF
            key[j] = s
        bitgen = np.random.MT19937()
        bitgen.state = {"bit_generator": "MT19937", "state": {"key": key[1:], "pos": 624}}
        _state["bitgen"] = bitgen
    bitgen = _state["bitgen"]

    def unif():
        u = float(bitgen.random_raw()) * 2.3283064365386963e-10
        return min(max(u, 1.164153218540399e-10), 1 - 1.164153218540399e-10)

    big = 134217728  # 2^27
    z = NormalDist()
    out = np.empty(n)
    for i in range(n):
        u = int(big * unif()) + unif()
        out[i] = z.inv_cdf(u / big)
    return mean + sd * out


# --- colours --------------------------------------------------------------

def _srgb_to_lab(rgb):
    rgb = np.asarray(rgb, float)
    lin = np.where(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055) ** 2.4)
    m = np.array([[0.4124564, 0.3575761, 0.1804375],
                  [0.2126729, 0.7151522, 0.0721750],
                  [0.0193339, 0.1191920, 0.9503041]])
    xyz = lin @ m.T / np.array([0.95047, 1.0, 1.08883])
    f = np.where(xyz > (6 / 29) ** 3, np.cbrt(xyz), xyz / (3 * (6 / 29) ** 2) + 4 / 29)
    return np.stack([116 * f[..., 1] - 16, 500 * (f[..., 0] - f[..., 1]),
                     200 * (f[..., 1] - f[..., 2])], axis=-1)


def _lab_to_srgb(lab):
    lab = np.asarray(lab, float)
    fy = (lab[..., 0] + 16) / 116
    f = np.stack([fy + lab[..., 1] / 500, fy, fy - lab[..., 2] / 200], axis=-1)
    xyz = np.where(f > 6 / 29, f ** 3, 3 * (6 / 29) ** 2 * (f - 4 / 29))
    xyz = xyz * np.array([0.95047, 1.0, 1.08883])
    m = np.array([[3.2404542, -1.5371385, -0.4985314],
                  [-0.9692660, 1.8760108, 0.0415560],
                  [0.0556434, -0.2040259, 1.0572252]])
    lin = np.clip(xyz @ m.T, 0, 1)
    return np.where(lin <= 0.0031308, 12.92 * lin, 1.055 * lin ** (1 / 2.4) - 0.055)


def gradient(colors, values, vmin=None, vmax=None):
    """RGB colours for `values` on an evenly spaced gradient through `colors`,
    interpolated in CIELAB like ggplot2's scale_colour_gradientn()."""
    values = np.asarray(values, float)
    vmin = values.min() if vmin is None else vmin
    vmax = values.max() if vmax is None else vmax
    t = np.clip((values - vmin) / (vmax - vmin), 0, 1)
    lab = _srgb_to_lab([to_rgb(c) for c in colors])
    stops = np.linspace(0, 1, len(colors))
    out = np.stack([np.interp(t, stops, lab[:, k]) for k in range(3)], axis=-1)
    return _lab_to_srgb(out)


def gradient_line(ax, x, y, colors, linewidth, vmin=None, vmax=None, alpha=1.0):
    """A line whose colour follows x along the gradient (round caps, like ggplot)."""
    x, y = np.asarray(x, float), np.asarray(y, float)
    pts = np.column_stack([x, y]).reshape(-1, 1, 2)
    segs = np.concatenate([pts[:-1], pts[1:]], axis=1)
    # ggplot2 colours each segment of geom_line() by its starting point
    cols = gradient(colors, x[:-1], x.min() if vmin is None else vmin,
                    x.max() if vmax is None else vmax)
    lc = LineCollection(segs, colors=cols, linewidths=linewidth, alpha=alpha,
                        capstyle="round", joinstyle="round")
    ax.add_collection(lc)
    ax.update_datalim(np.column_stack([x, y]))
    return lc


def _text(ax, x, y, s, color, font, size, ha, angle):
    """Text vertically centred on the ink of its glyphs, as grid does with vjust = 0.5
    (matplotlib's va="center" centres the whole line box, which sits lower)."""
    fp = FontProperties(fname=font, size=size)
    ext = TextPath((0, 0), s, prop=fp).get_extents()
    off = (ext.y0 + ext.y1) / 2  # points above the baseline
    th = np.deg2rad(angle)
    tr = offset_copy(ax.transData, fig=ax.figure, x=off * np.sin(th), y=-off * np.cos(th),
                     units="points")
    ax.text(x, y, s, color=color, ha=ha, va="baseline", rotation=angle,
            rotation_mode="anchor", fontproperties=fp, transform=tr, zorder=3)


# --- sticker --------------------------------------------------------------

def sticker(subplot=None, *, s_x=0.8, s_y=0.75, s_width=0.4, s_height=0.5,
            package, p_x=1, p_y=1.4, p_color="#FFFFFF", p_family="Roboto Mono",
            p_weight=400, p_size=8,
            h_size=2.56, h_fill="#1881C2", h_color="#87B13F",
            url="", u_x=1, u_y=0.08, u_color="black", u_family="Roboto Mono",
            u_weight=400, u_size=1.5, u_angle=30,
            filename=None, dpi=300):
    """Draw and save a hex sticker.

    subplot: callable(ax) drawing into an axes placed at (s_x, s_y) with size
    s_width x s_height in hex units (the hexagon spans 0..2 vertically).
    Data margins default to 5 %, as in ggplot2; axes are hidden.
    """
    fig = plt.figure(figsize=(WIDTH_MM / 25.4, HEIGHT_MM / 25.4))
    ax = fig.add_axes((0, 0, 1, 1))
    m = 1.05
    ax.set_xlim(1 - _HALF_W * m, 1 + _HALF_W * m)
    ax.set_ylim(1 - m, 1 + m)
    ax.set_aspect("equal")
    ax.set_axis_off()

    verts = np.column_stack([HEX_X, HEX_Y])
    ax.add_patch(Polygon(verts, closed=True, facecolor=h_fill, edgecolor="none", zorder=0))

    if subplot is not None:
        sub = ax.inset_axes((s_x - s_width / 2, s_y - s_height / 2, s_width, s_height),
                            transform=ax.transData, zorder=1)
        sub.set_axis_off()
        sub.patch.set_alpha(0)
        sub.margins(0.05)
        subplot(sub)
        sub.autoscale_view()

    ax.add_patch(Polygon(verts, closed=True, facecolor="none", edgecolor=h_color,
                         linewidth=h_size, joinstyle="round", zorder=2))

    _text(ax, p_x, p_y, package, p_color, google_font(p_family, p_weight), p_size, "center", 0)
    if url:
        _text(ax, u_x, u_y, url, u_color, google_font(u_family, u_weight), u_size, "left", u_angle)

    filename = filename or ROOT / "logo" / "python" / f"{package}.png"
    fig.savefig(filename, dpi=dpi, transparent=True)
    plt.close(fig)
    print(f"saved {filename}")
    return Path(filename)
