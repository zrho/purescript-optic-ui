module OpticUI.Markup.SVG where
--------------------------------------------------------------------------------
import OpticUI.Markup (Markup (), Prop (), element, attr)
import Data.Maybe (Maybe (Just))
--------------------------------------------------------------------------------

svgNamespace :: Maybe String
svgNamespace = Just "http://www.w3.org/2000/svg"

a :: Array Prop -> Markup -> Markup
a = element svgNamespace "a"

altGlyph :: Array Prop -> Markup -> Markup
altGlyph = element svgNamespace "altGlyph"

altGlyphDef :: Array Prop -> Markup -> Markup
altGlyphDef = element svgNamespace "altGlyphDef"

altGlyphItem :: Array Prop -> Markup -> Markup
altGlyphItem = element svgNamespace "altGlyphItem"

animate :: Array Prop -> Markup -> Markup
animate = element svgNamespace "animate"

animateColor :: Array Prop -> Markup -> Markup
animateColor = element svgNamespace "animateColor"

animateMotion :: Array Prop -> Markup -> Markup
animateMotion = element svgNamespace "animateMotion"

animateTransform :: Array Prop -> Markup -> Markup
animateTransform = element svgNamespace "animateTransform"

circle :: Array Prop -> Markup -> Markup
circle = element svgNamespace "circle"

clipPath :: Array Prop -> Markup -> Markup
clipPath = element svgNamespace "clipPath"

colorProfile :: Array Prop -> Markup -> Markup
colorProfile = element svgNamespace "color-profile"

cursor :: Array Prop -> Markup -> Markup
cursor = element svgNamespace "cursor"

defs :: Array Prop -> Markup -> Markup
defs = element svgNamespace "defs"

desc :: Array Prop -> Markup -> Markup
desc = element svgNamespace "desc"

ellipse :: Array Prop -> Markup -> Markup
ellipse = element svgNamespace "ellipse"

feBlend :: Array Prop -> Markup -> Markup
feBlend = element svgNamespace "feBlend"

feColorMatrix :: Array Prop -> Markup -> Markup
feColorMatrix = element svgNamespace "feColorMatrix"

feComponentTransfer :: Array Prop -> Markup -> Markup
feComponentTransfer = element svgNamespace "feComponentTransfer"

feComposite :: Array Prop -> Markup -> Markup
feComposite = element svgNamespace "feComposite"

feConvolveMatrix :: Array Prop -> Markup -> Markup
feConvolveMatrix = element svgNamespace "feConvolveMatrix"

feDiffuseLighting :: Array Prop -> Markup -> Markup
feDiffuseLighting = element svgNamespace "feDiffuseLighting"

feDisplacementMap :: Array Prop -> Markup -> Markup
feDisplacementMap = element svgNamespace "feDisplacementMap"

feDistantLight :: Array Prop -> Markup -> Markup
feDistantLight = element svgNamespace "feDistantLight"

feFlood :: Array Prop -> Markup -> Markup
feFlood = element svgNamespace "feFlood"

feFuncA :: Array Prop -> Markup -> Markup
feFuncA = element svgNamespace "feFuncA"

feFuncB :: Array Prop -> Markup -> Markup
feFuncB = element svgNamespace "feFuncB"

feFuncG :: Array Prop -> Markup -> Markup
feFuncG = element svgNamespace "feFuncG"

feFuncR :: Array Prop -> Markup -> Markup
feFuncR = element svgNamespace "feFuncR"

feGaussianBlur :: Array Prop -> Markup -> Markup
feGaussianBlur = element svgNamespace "feGaussianBlur"

feImage :: Array Prop -> Markup -> Markup
feImage = element svgNamespace "feImage"

feMerge :: Array Prop -> Markup -> Markup
feMerge = element svgNamespace "feMerge"

feMergeNode :: Array Prop -> Markup -> Markup
feMergeNode = element svgNamespace "feMergeNode"

feMorphology :: Array Prop -> Markup -> Markup
feMorphology = element svgNamespace "feMorphology"

feOffset :: Array Prop -> Markup -> Markup
feOffset = element svgNamespace "feOffset"

fePointLight :: Array Prop -> Markup -> Markup
fePointLight = element svgNamespace "fePointLight"

feSpecularLighting :: Array Prop -> Markup -> Markup
feSpecularLighting = element svgNamespace "feSpecularLighting"

feSpotLight :: Array Prop -> Markup -> Markup
feSpotLight = element svgNamespace "feSpotLight"

feTile :: Array Prop -> Markup -> Markup
feTile = element svgNamespace "feTile"

feTurbulence :: Array Prop -> Markup -> Markup
feTurbulence = element svgNamespace "feTurbulence"

filter :: Array Prop -> Markup -> Markup
filter = element svgNamespace "filter"

font :: Array Prop -> Markup -> Markup
font = element svgNamespace "font"

fontFace :: Array Prop -> Markup -> Markup
fontFace = element svgNamespace "font-face"

fontFaceFormat :: Array Prop -> Markup -> Markup
fontFaceFormat = element svgNamespace "font-face-format"

fontFaceName :: Array Prop -> Markup -> Markup
fontFaceName = element svgNamespace "font-face-name"

fontFaceSrc :: Array Prop -> Markup -> Markup
fontFaceSrc = element svgNamespace "font-face-src"

fontFaceUri :: Array Prop -> Markup -> Markup
fontFaceUri = element svgNamespace "font-face-uri"

foreignObject :: Array Prop -> Markup -> Markup
foreignObject = element svgNamespace "foreignObject"

g :: Array Prop -> Markup -> Markup
g = element svgNamespace "g"

glyph :: Array Prop -> Markup -> Markup
glyph = element svgNamespace "glyph"

glyphRef :: Array Prop -> Markup -> Markup
glyphRef = element svgNamespace "glyphRef"

hkern :: Array Prop -> Markup -> Markup
hkern = element svgNamespace "hkern"

image :: Array Prop -> Markup -> Markup
image = element svgNamespace "image"

line :: Array Prop -> Markup -> Markup
line = element svgNamespace "line"

linearGradient :: Array Prop -> Markup -> Markup
linearGradient = element svgNamespace "linearGradient"

marker :: Array Prop -> Markup -> Markup
marker = element svgNamespace "marker"

mask :: Array Prop -> Markup -> Markup
mask = element svgNamespace "mask"

metadata :: Array Prop -> Markup -> Markup
metadata = element svgNamespace "metadata"

missingGlyph :: Array Prop -> Markup -> Markup
missingGlyph = element svgNamespace "missing-glyph"

mpath :: Array Prop -> Markup -> Markup
mpath = element svgNamespace "mpath"

path :: Array Prop -> Markup -> Markup
path = element svgNamespace "path"

pattern :: Array Prop -> Markup -> Markup
pattern = element svgNamespace "pattern"

polygon :: Array Prop -> Markup -> Markup
polygon = element svgNamespace "polygon"

polyline :: Array Prop -> Markup -> Markup
polyline = element svgNamespace "polyline"

radialGradient :: Array Prop -> Markup -> Markup
radialGradient = element svgNamespace "radialGradient"

rect :: Array Prop -> Markup -> Markup
rect = element svgNamespace "rect"

script :: Array Prop -> Markup -> Markup
script = element svgNamespace "script"

set :: Array Prop -> Markup -> Markup
set = element svgNamespace "set"

stop :: Array Prop -> Markup -> Markup
stop = element svgNamespace "stop"

style :: Array Prop -> Markup -> Markup
style = element svgNamespace "style"

svg :: Array Prop -> Markup -> Markup
svg = element svgNamespace "svg"

switch :: Array Prop -> Markup -> Markup
switch = element svgNamespace "switch"

symbol :: Array Prop -> Markup -> Markup
symbol = element svgNamespace "symbol"

text :: Array Prop -> Markup -> Markup
text = element svgNamespace "text"

textPath :: Array Prop -> Markup -> Markup
textPath = element svgNamespace "textPath"

title :: Array Prop -> Markup -> Markup
title = element svgNamespace "title"

tref :: Array Prop -> Markup -> Markup
tref = element svgNamespace "tref"

tspan :: Array Prop -> Markup -> Markup
tspan = element svgNamespace "tspan"

use :: Array Prop -> Markup -> Markup
use = element svgNamespace "use"

view :: Array Prop -> Markup -> Markup
view = element svgNamespace "view"

vkern :: Array Prop -> Markup -> Markup
vkern = element svgNamespace "vkern"

--------------------------------------------------------------------------------
-- Attributes

textA :: String -> Prop
textA = attr "text"

transformA :: String -> Prop
transformA = attr "transform"

widthA :: String -> Prop
widthA = attr "width"

heightA :: String -> Prop
heightA = attr "height"

xA :: String -> Prop
xA = attr "x"

yA :: String -> Prop
yA = attr "y"

dyA :: String -> Prop
dyA = attr "dy"

dA :: String -> Prop
dA = attr "d"

fillA :: String -> Prop
fillA = attr "fill"

strokeA :: String -> Prop
strokeA = attr "stroke"

strokeWidthA :: String -> Prop
strokeWidthA = attr "stroke-width"
