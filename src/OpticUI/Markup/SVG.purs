module OpticUI.Markup.SVG where
--------------------------------------------------------------------------------
import OpticUI.Markup (Markup (), Prop (), element, attr)
import Data.Maybe (Maybe (Nothing))
import Data.Monoid (mempty)
--------------------------------------------------------------------------------

a :: forall m. Array Prop -> Markup -> Markup
a = element Nothing "a"

altGlyph :: forall m. Array Prop -> Markup -> Markup
altGlyph = element Nothing "altGlyph"

altGlyphDef :: forall m. Array Prop -> Markup -> Markup
altGlyphDef = element Nothing "altGlyphDef"

altGlyphItem :: forall m. Array Prop -> Markup -> Markup
altGlyphItem = element Nothing "altGlyphItem"

animate :: forall m. Array Prop -> Markup -> Markup
animate = element Nothing "animate"

animateColor :: forall m. Array Prop -> Markup -> Markup
animateColor = element Nothing "animateColor"

animateMotion :: forall m. Array Prop -> Markup -> Markup
animateMotion = element Nothing "animateMotion"

animateTransform :: forall m. Array Prop -> Markup -> Markup
animateTransform = element Nothing "animateTransform"

circle :: forall m. Array Prop -> Markup -> Markup
circle = element Nothing "circle"

clipPath :: forall m. Array Prop -> Markup -> Markup
clipPath = element Nothing "clipPath"

colorProfile :: forall m. Array Prop -> Markup -> Markup
colorProfile = element Nothing "color-profile"

cursor :: forall m. Array Prop -> Markup -> Markup
cursor = element Nothing "cursor"

defs :: forall m. Array Prop -> Markup -> Markup
defs = element Nothing "defs"

desc :: forall m. Array Prop -> Markup -> Markup
desc = element Nothing "desc"

ellipse :: forall m. Array Prop -> Markup -> Markup
ellipse = element Nothing "ellipse"

feBlend :: forall m. Array Prop -> Markup -> Markup
feBlend = element Nothing "feBlend"

feColorMatrix :: forall m. Array Prop -> Markup -> Markup
feColorMatrix = element Nothing "feColorMatrix"

feComponentTransfer :: forall m. Array Prop -> Markup -> Markup
feComponentTransfer = element Nothing "feComponentTransfer"

feComposite :: forall m. Array Prop -> Markup -> Markup
feComposite = element Nothing "feComposite"

feConvolveMatrix :: forall m. Array Prop -> Markup -> Markup
feConvolveMatrix = element Nothing "feConvolveMatrix"

feDiffuseLighting :: forall m. Array Prop -> Markup -> Markup
feDiffuseLighting = element Nothing "feDiffuseLighting"

feDisplacementMap :: forall m. Array Prop -> Markup -> Markup
feDisplacementMap = element Nothing "feDisplacementMap"

feDistantLight :: forall m. Array Prop -> Markup -> Markup
feDistantLight = element Nothing "feDistantLight"

feFlood :: forall m. Array Prop -> Markup -> Markup
feFlood = element Nothing "feFlood"

feFuncA :: forall m. Array Prop -> Markup -> Markup
feFuncA = element Nothing "feFuncA"

feFuncB :: forall m. Array Prop -> Markup -> Markup
feFuncB = element Nothing "feFuncB"

feFuncG :: forall m. Array Prop -> Markup -> Markup
feFuncG = element Nothing "feFuncG"

feFuncR :: forall m. Array Prop -> Markup -> Markup
feFuncR = element Nothing "feFuncR"

feGaussianBlur :: forall m. Array Prop -> Markup -> Markup
feGaussianBlur = element Nothing "feGaussianBlur"

feImage :: forall m. Array Prop -> Markup -> Markup
feImage = element Nothing "feImage"

feMerge :: forall m. Array Prop -> Markup -> Markup
feMerge = element Nothing "feMerge"

feMergeNode :: forall m. Array Prop -> Markup -> Markup
feMergeNode = element Nothing "feMergeNode"

feMorphology :: forall m. Array Prop -> Markup -> Markup
feMorphology = element Nothing "feMorphology"

feOffset :: forall m. Array Prop -> Markup -> Markup
feOffset = element Nothing "feOffset"

fePointLight :: forall m. Array Prop -> Markup -> Markup
fePointLight = element Nothing "fePointLight"

feSpecularLighting :: forall m. Array Prop -> Markup -> Markup
feSpecularLighting = element Nothing "feSpecularLighting"

feSpotLight :: forall m. Array Prop -> Markup -> Markup
feSpotLight = element Nothing "feSpotLight"

feTile :: forall m. Array Prop -> Markup -> Markup
feTile = element Nothing "feTile"

feTurbulence :: forall m. Array Prop -> Markup -> Markup
feTurbulence = element Nothing "feTurbulence"

filter :: forall m. Array Prop -> Markup -> Markup
filter = element Nothing "filter"

font :: forall m. Array Prop -> Markup -> Markup
font = element Nothing "font"

fontFace :: forall m. Array Prop -> Markup -> Markup
fontFace = element Nothing "font-face"

fontFaceFormat :: forall m. Array Prop -> Markup -> Markup
fontFaceFormat = element Nothing "font-face-format"

fontFaceName :: forall m. Array Prop -> Markup -> Markup
fontFaceName = element Nothing "font-face-name"

fontFaceSrc :: forall m. Array Prop -> Markup -> Markup
fontFaceSrc = element Nothing "font-face-src"

fontFaceUri :: forall m. Array Prop -> Markup -> Markup
fontFaceUri = element Nothing "font-face-uri"

foreignObject :: forall m. Array Prop -> Markup -> Markup
foreignObject = element Nothing "foreignObject"

g :: forall m. Array Prop -> Markup -> Markup
g = element Nothing "g"

glyph :: forall m. Array Prop -> Markup -> Markup
glyph = element Nothing "glyph"

glyphRef :: forall m. Array Prop -> Markup -> Markup
glyphRef = element Nothing "glyphRef"

hkern :: forall m. Array Prop -> Markup -> Markup
hkern = element Nothing "hkern"

image :: forall m. Array Prop -> Markup -> Markup
image = element Nothing "image"

line :: forall m. Array Prop -> Markup -> Markup
line = element Nothing "line"

linearGradient :: forall m. Array Prop -> Markup -> Markup
linearGradient = element Nothing "linearGradient"

marker :: forall m. Array Prop -> Markup -> Markup
marker = element Nothing "marker"

mask :: forall m. Array Prop -> Markup -> Markup
mask = element Nothing "mask"

metadata :: forall m. Array Prop -> Markup -> Markup
metadata = element Nothing "metadata"

missingGlyph :: forall m. Array Prop -> Markup -> Markup
missingGlyph = element Nothing "missing-glyph"

mpath :: forall m. Array Prop -> Markup -> Markup
mpath = element Nothing "mpath"

path :: forall m. Array Prop -> Markup -> Markup
path = element Nothing "path"

pattern :: forall m. Array Prop -> Markup -> Markup
pattern = element Nothing "pattern"

polygon :: forall m. Array Prop -> Markup -> Markup
polygon = element Nothing "polygon"

polyline :: forall m. Array Prop -> Markup -> Markup
polyline = element Nothing "polyline"

radialGradient :: forall m. Array Prop -> Markup -> Markup
radialGradient = element Nothing "radialGradient"

rect :: forall m. Array Prop -> Markup -> Markup
rect = element Nothing "rect"

script :: forall m. Array Prop -> Markup -> Markup
script = element Nothing "script"

set :: forall m. Array Prop -> Markup -> Markup
set = element Nothing "set"

stop :: forall m. Array Prop -> Markup -> Markup
stop = element Nothing "stop"

style :: forall m. Array Prop -> Markup -> Markup
style = element Nothing "style"

svg :: forall m. Array Prop -> Markup -> Markup
svg = element Nothing "svg"

switch :: forall m. Array Prop -> Markup -> Markup
switch = element Nothing "switch"

symbol :: forall m. Array Prop -> Markup -> Markup
symbol = element Nothing "symbol"

text :: forall m. Array Prop -> Markup -> Markup
text = element Nothing "text"

textPath :: forall m. Array Prop -> Markup -> Markup
textPath = element Nothing "textPath"

title :: forall m. Array Prop -> Markup -> Markup
title = element Nothing "title"

tref :: forall m. Array Prop -> Markup -> Markup
tref = element Nothing "tref"

tspan :: forall m. Array Prop -> Markup -> Markup
tspan = element Nothing "tspan"

use :: forall m. Array Prop -> Markup -> Markup
use = element Nothing "use"

view :: forall m. Array Prop -> Markup -> Markup
view = element Nothing "view"

vkern :: forall m. Array Prop -> Markup -> Markup
vkern = element Nothing "vkern"

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
