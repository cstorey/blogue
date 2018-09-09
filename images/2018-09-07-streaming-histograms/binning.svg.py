import lxml.etree
from lxml.builder import ElementMaker

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

pix=chart_width*1/4
basey=chart_height
miy=chart_height*3/4
pbx=chart_width*2/4
mby=chart_height*2/4
pjx=chart_width*3/4
mjy=chart_height*1/4
bin_width = chart_width*1/8

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
  S.text(
    dict(x=str(axes_offset/4), y=str(miy)),
    "Mi",
  ),
  S.text(
    dict(x=str(axes_offset/4), y=str(mby)),
    "Mb",
  ),
  S.text(
    dict(x=str(axes_offset/4), y=str(mjy)),
    "Mi+1",
  ),
)

# X labels
x_labels = S.g(
  dict(transform="translate({}, {})".format(0, chart_height)),
  S.text(
    dict(y=str(axes_offset/4), x=str(pix)),
    "Pi",
  ),
  S.text(
    dict(y=str(axes_offset/4), x=str(pbx)),
    "Pb",
  ),
  S.text(
    dict(y=str(axes_offset/4), x=str(pjx)),
    "Pi+1",
  ),
)
# Bins

bins = S.g(
  S.line(
    dict(id="bin-i", stroke="black"),
	 x1=str(pix), y1=str(basey),
	 x2=str(pix), y2=str(miy)),
  S.line(
    dict(id="bin-i+1", stroke="black"),
	 x1=str(pjx), y1=str(basey),
	 x2=str(pjx), y2=str(mjy)),
  S.rect(
    dict(id="rect-i", fill="hsla(96, 100%, 50%, 0.2)"),
	 x=str(pix-(bin_width/2)), width=str(bin_width),
	 y=str(miy), height=str(chart_height-miy)),
  S.rect(
    dict(id="rect-i", fill="url(#diagonalHatch)"),
	 x=str(pix-(bin_width/2)), width=str(bin_width/2),
	 y=str(miy), height=str(chart_height-miy)),
  S.polygon(
    dict(id="trapezoid-i-b", fill="hsla(248, 100%, 50%, 0.2)"),
    points="{pix},{basey} {pix},{miy} {pbx},{mby} {pbx},{basey}".format(
      pix=pix, basey=basey,miy=miy,pbx=pbx,mby=mby,
    ),
  )
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

#images = []
#for x in xrange(4):
#    for y in xrange(4):
#        ix0 = x*s
#        iy0 = y*s
#        ix1 = (x+1)*s
#        iy1 = (y+1)*s
#
#        images.append(S.image({
#          'x': ix0,
#          'y': iy0,
#          'height': (ix1-ix0),
#          'width': (iy1-iy0),
#          '{%s}%s' % (XL_NS, 'href'): 'http://tile.openstreetmap.org/%d/%d/%d.png' % (z, x+xoff, y+yoff)
#          }))
print lxml.etree.tostring(svg, pretty_print=True)
