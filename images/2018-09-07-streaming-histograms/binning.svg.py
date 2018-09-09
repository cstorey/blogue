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

# Main chart area
svg.append(
  S.g(
    dict(transform="translate({}, {})".format(axes_offset, axes_offset)),
    # Axes
    S.path(
      d="M 0,0 L 0,{height} L {width},{height}".format(height=chart_height, width=chart_width),
      stroke="black", fill="none"),
    # Y labels
    S.g(
      dict(transform="translate({}, {})".format(-axes_offset, 0)),
      S.text(
	dict(x=str(axes_offset/4), y=str(chart_height*1/4)),
	"Mi+1",
      ),
      S.text(
	dict(x=str(axes_offset/4), y=str(chart_height*2/4)),
	"Mb",
      ),
      S.text(
	dict(x=str(axes_offset/4), y=str(chart_height*3/4)),
	"Mi",
      ),
    ),
    # X labels
    S.g(
      dict(transform="translate({}, {})".format(0, chart_height)),
      S.text(
	dict(y=str(axes_offset/4), x=str(chart_width*1/4)),
	"Pi+1",
      ),
      S.text(
	dict(y=str(axes_offset/4), x=str(chart_width*2/4)),
	"Pb",
      ),
      S.text(
	dict(y=str(axes_offset/4), x=str(chart_width*3/4)),
	"Pi",
      ),
    ),
    # Bins
    S.g(
      S.line(
	dict(id="bin-i", stroke="black"),
	     x1=str(chart_width*1/4), y1=str(chart_height),
	     x2=str(chart_width*1/4), y2=str(chart_height*3/4)),
      S.line(
	dict(id="bin-i+1", stroke="black"),
	     x1=str(chart_width*3/4), y1=str(chart_height),
	     x2=str(chart_width*3/4), y2=str(chart_height*1/4)),
      S.rect(
	dict(id="rect-i", fill="hsla(96, 100%, 50%, 0.2)"),
	     x=str(chart_width*1/8), width=str((chart_width*1/4)),
	     y=str(chart_height*3/4), height=str(chart_height*1/4)),
      S.rect(
	dict(id="rect-i", fill="url(#diagonalHatch)"),
	     x=str(chart_width*1/8), width=str((chart_width*1/8)),
	     y=str(chart_height*3/4), height=str(chart_height*1/4)),
      S.polygon(
	dict(id="trapezoid-i-b", fill="hsla(248, 100%, 50%, 0.2)"),
	points="{pix},{basey} {pix},{mix} {pbx},{mby} {pbx},{basey}".format(
	  pix=chart_width*1/4,
	  basey=chart_height,
	  mix=chart_height*3/4,
	  pbx=chart_width*2/4,
	  mby=chart_height*2/4,
	),
      )
    ),
  ),
)

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
