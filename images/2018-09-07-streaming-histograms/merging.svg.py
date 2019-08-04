from lxml import etree
from lxml.builder import ElementMaker
import sys, os

SVG_NS = "http://www.w3.org/2000/svg"
XL_NS = "http://www.w3.org/1999/xlink"
S = ElementMaker(
      namespace=SVG_NS,
      nsmap={None: SVG_NS, 'xl' : XL_NS},
      typemap={int: lambda e, i: str(i)})

width=640
height=width*3/4

axes_offset = 64
chart_height=height-2* axes_offset
chart_width=width-2* axes_offset

def x(frac):
  return chart_width * frac
def y(frac):
  return chart_height * (1.0 - frac) 

basey=y(0.0)
p0x=x(1./8)
m0y=y(1.5/4)
p1x=x(2./8)
m1y=y(.5/4)
p2x=p1x + chart_width/8
pi = 5./8
pix=x(pi)
mi = 1./4
miy=y(mi)
pj = 6./8
pjx=x(pj)
mj = 3./4
mjy=y(mj)
pnx=x(8./8)
mny=y(.5/4)

pmx = x((pi*mi + pj*mj) / (mi+mj))
mmy = y(mi+mj)

svg = S.svg(dict(width=str(width), height=str(height)))

svg.append(
  S.defs(
    S.pattern(
      dict(
	id="diagonalHatch",
	patternUnits="userSpaceOnUse",
	width="8",
	height="8",
      ),
      S.path(
	d="M-1,1 l2,-2 M0,8 l8,-8 M7,9 l2,-2",
	style="stroke:hsla(0, 0%, 0%, 0.34); stroke-width:1",
      ),
    )
  )
)

# Axes
axes = S.path(
    d="M 0,0 L 0,{height} L {width},{height}".format(
      height=chart_height, width=chart_width),
    stroke="black", fill="none")
# Y labels
y_labels = S.g(
  dict(transform="translate({}, {})".format(-axes_offset, 0)),
  S.text( dict(x=str(axes_offset/4), y=str(y(0./4))), "0"),
  S.text( dict(x=str(axes_offset/4), y=str(y(1./4))), "1"),
  S.text( dict(x=str(axes_offset/4), y=str(y(2./4))), "2"),
  S.text( dict(x=str(axes_offset/4), y=str(y(3./4))), "3"),
  S.text( dict(x=str(axes_offset/4), y=str(y(4./4))), "4"),
)

# X labels
x_labels = S.g(
  dict(transform="translate({}, {})".format(0, chart_height)),
  S.text( dict(y=str(axes_offset/4), x=str(p0x)), "P0",),
  S.text( dict(y=str(axes_offset/4), x=str(p1x)), "P1",),
  S.text( dict(y=str(axes_offset/4), x=str(p2x)), "...",),
  S.text( dict(y=str(axes_offset/4), x=str(pix)), "Pi",),
  S.text( dict(y=str(axes_offset/4), x=str(pjx)), "Pi+1",),
  S.text( dict(y=str(axes_offset/4), x=str(pnx)), "Pn",),
)
# Bins

bins = S.g(
  S.line(
    dict(id="bin-0", stroke="black"),
	 x1=str(p0x), y1=str(basey),
	 x2=str(p0x), y2=str(m0y)),
  S.line(
    dict(id="bin-1", stroke="black"),
	 x1=str(p1x), y1=str(basey),
	 x2=str(p1x), y2=str(m1y)),
  S.line({'id':"bin-i+1", 'stroke': "black", 'stroke-dasharray': '4 4'},
	 x1=str(pix), y1=str(basey),
	 x2=str(pix), y2=str(miy)),
  S.line({'id':"bin-i+1", 'stroke': "black", 'stroke-dasharray': '4 4'},
	 x1=str(pjx), y1=str(basey),
	 x2=str(pjx), y2=str(mjy)),
  S.line(
    dict(id="bin-merged", stroke="black"),
	 x1=str(pmx), y1=str(basey),
	 x2=str(pmx), y2=str(mmy)),

  S.line(
    dict(id="bin-n", stroke="black"),
	 x1=str(pnx), y1=str(basey),
	 x2=str(pnx), y2=str(mny)),
)
# Main chart area
chart_area = S.g(
  dict(transform="translate({}, {})".format(axes_offset, axes_offset)),
  axes,
  y_labels,
  x_labels,
  bins,
)
svg.append(chart_area)

with os.fdopen(sys.stdout.fileno(), 'wb', closefd=False) as of:
  etree.ElementTree(svg).write(of, pretty_print=True)

