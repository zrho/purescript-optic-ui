module OpticUI.Markup.HTML where
--------------------------------------------------------------------------------
import Prelude hiding (sub, div, map)
import OpticUI.Markup (Markup (), Prop (), Event (), KeyboardEvent (), element, handle, attr, prop, UniqueStr (),
                       initializer, finalizer)
import Data.Maybe         (Maybe (..), maybe)
import Data.Either        (either)
import Data.Monoid        (mempty)
import Data.Foreign       (toForeign)
import Data.Foreign.Class (IsForeign, readProp)
import Control.Monad.Eff  (Eff())
import DOM.HTML.Types     (HTMLElement ())
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Elements

a :: Array Prop -> Markup -> Markup
a = element Nothing "a"

abbr :: Array Prop -> Markup -> Markup
abbr = element Nothing "abbr"

address :: Array Prop -> Markup -> Markup
address = element Nothing "address"

area :: Array Prop -> Markup -> Markup
area = element Nothing "area"

article :: Array Prop -> Markup -> Markup
article = element Nothing "article"

aside :: Array Prop -> Markup -> Markup
aside = element Nothing "aside"

audio :: Array Prop -> Markup -> Markup
audio = element Nothing "audio"

b :: Array Prop -> Markup -> Markup
b = element Nothing "b"

base :: Array Prop -> Markup -> Markup
base = element Nothing "base"

bdi :: Array Prop -> Markup -> Markup
bdi = element Nothing "bdi"

bdo :: Array Prop -> Markup -> Markup
bdo = element Nothing "bdo"

blockquote :: Array Prop -> Markup -> Markup
blockquote = element Nothing "blockquote"

body :: Array Prop -> Markup -> Markup
body = element Nothing "body"

br :: Array Prop -> Markup -> Markup
br = element Nothing "br"

button :: Array Prop -> Markup -> Markup
button = element Nothing "button"

canvas :: Array Prop -> Markup -> Markup
canvas = element Nothing "canvas"

caption :: Array Prop -> Markup -> Markup
caption = element Nothing "caption"

cite :: Array Prop -> Markup -> Markup
cite = element Nothing "cite"

code :: Array Prop -> Markup -> Markup
code = element Nothing "code"

col :: Array Prop -> Markup -> Markup
col = element Nothing "col"

colgroup :: Array Prop -> Markup -> Markup
colgroup = element Nothing "colgroup"

dataTag :: Array Prop -> Markup -> Markup
dataTag = element Nothing "data"

datalist :: Array Prop -> Markup -> Markup
datalist = element Nothing "datalist"

dd :: Array Prop -> Markup -> Markup
dd = element Nothing "dd"

del :: Array Prop -> Markup -> Markup
del = element Nothing "del"

details :: Array Prop -> Markup -> Markup
details = element Nothing "details"

dfn :: Array Prop -> Markup -> Markup
dfn = element Nothing "dfn"

dialog :: Array Prop -> Markup -> Markup
dialog = element Nothing "dialog"

div :: Array Prop -> Markup -> Markup
div = element Nothing "div"

div_ :: Markup -> Markup
div_ = element Nothing "div" []

dl :: Array Prop -> Markup -> Markup
dl = element Nothing "dl"

dt :: Array Prop -> Markup -> Markup
dt = element Nothing "dt"

em :: Array Prop -> Markup -> Markup
em = element Nothing "em"

embed :: Array Prop -> Markup -> Markup
embed = element Nothing "embed"

fieldset :: Array Prop -> Markup -> Markup
fieldset = element Nothing "fieldset"

figcaption :: Array Prop -> Markup -> Markup
figcaption = element Nothing "figcaption"

figure :: Array Prop -> Markup -> Markup
figure = element Nothing "figure"

footer :: Array Prop -> Markup -> Markup
footer = element Nothing "footer"

form :: Array Prop -> Markup -> Markup
form = element Nothing "form"

h1 :: Array Prop -> Markup -> Markup
h1 = element Nothing "h1"

h2 :: Array Prop -> Markup -> Markup
h2 = element Nothing "h2"

h3 :: Array Prop -> Markup -> Markup
h3 = element Nothing "h3"

h4 :: Array Prop -> Markup -> Markup
h4 = element Nothing "h4"

h5 :: Array Prop -> Markup -> Markup
h5 = element Nothing "h5"

h6 :: Array Prop -> Markup -> Markup
h6 = element Nothing "h6"

h1_ :: Markup -> Markup
h1_ = element Nothing "h1" []

h2_ :: Markup -> Markup
h2_ = element Nothing "h2" []

h3_ :: Markup -> Markup
h3_ = element Nothing "h3" []

h4_ :: Markup -> Markup
h4_ = element Nothing "h4" []

h5_ :: Markup -> Markup
h5_ = element Nothing "h5" []

h6_ :: Markup -> Markup
h6_ = element Nothing "h6" []

head :: Array Prop -> Markup -> Markup
head = element Nothing "head"

header :: Array Prop -> Markup -> Markup
header = element Nothing "header"

hgroup :: Array Prop -> Markup -> Markup
hgroup = element Nothing "hgroup"

hr :: Array Prop -> Markup -> Markup
hr = element Nothing "hr"

html :: Array Prop -> Markup -> Markup
html = element Nothing "html"

i :: Array Prop -> Markup -> Markup
i = element Nothing "i"

iframe :: Array Prop -> Markup -> Markup
iframe = element Nothing "iframe"

img :: Array Prop -> Markup -> Markup
img = element Nothing "img"

input :: Array Prop -> Markup -> Markup
input = element Nothing "input"

input_ :: Array Prop -> Markup
input_ ps = element Nothing "input" ps mempty

ins :: Array Prop -> Markup -> Markup
ins = element Nothing "ins"

kbd :: Array Prop -> Markup -> Markup
kbd = element Nothing "kbd"

keygen :: Array Prop -> Markup -> Markup
keygen = element Nothing "keygen"

label :: Array Prop -> Markup -> Markup
label = element Nothing "label"

legend :: Array Prop -> Markup -> Markup
legend = element Nothing "legend"

li :: Array Prop -> Markup -> Markup
li = element Nothing "li"

li_ :: Markup -> Markup
li_ = element Nothing "li" []

link :: Array Prop -> Markup -> Markup
link = element Nothing "link"

main :: Array Prop -> Markup -> Markup
main = element Nothing "main"

map :: Array Prop -> Markup -> Markup
map = element Nothing "map"

mark :: Array Prop -> Markup -> Markup
mark = element Nothing "mark"

menu :: Array Prop -> Markup -> Markup
menu = element Nothing "menu"

menuitem :: Array Prop -> Markup -> Markup
menuitem = element Nothing "menuitem"

meta :: Array Prop -> Markup -> Markup
meta = element Nothing "meta"

meter :: Array Prop -> Markup -> Markup
meter = element Nothing "meter"

nav :: Array Prop -> Markup -> Markup
nav = element Nothing "nav"

noscript :: Array Prop -> Markup -> Markup
noscript = element Nothing "noscript"

object :: Array Prop -> Markup -> Markup
object = element Nothing "object"

ol :: Array Prop -> Markup -> Markup
ol = element Nothing "ol"

optgroup :: Array Prop -> Markup -> Markup
optgroup = element Nothing "optgroup"

option :: Array Prop -> Markup -> Markup
option = element Nothing "option"

output :: Array Prop -> Markup -> Markup
output = element Nothing "output"

p :: Array Prop -> Markup -> Markup
p = element Nothing "p"

p_ :: Markup -> Markup
p_ = element Nothing "p" []

param :: Array Prop -> Markup -> Markup
param = element Nothing "param"

pre :: Array Prop -> Markup -> Markup
pre = element Nothing "pre"

progress :: Array Prop -> Markup -> Markup
progress = element Nothing "progress"

q :: Array Prop -> Markup -> Markup
q = element Nothing "q"

rb :: Array Prop -> Markup -> Markup
rb = element Nothing "rb"

rp :: Array Prop -> Markup -> Markup
rp = element Nothing "rp"

rt :: Array Prop -> Markup -> Markup
rt = element Nothing "rt"

rtc :: Array Prop -> Markup -> Markup
rtc = element Nothing "rtc"

ruby :: Array Prop -> Markup -> Markup
ruby = element Nothing "ruby"

s :: Array Prop -> Markup -> Markup
s = element Nothing "s"

samp :: Array Prop -> Markup -> Markup
samp = element Nothing "samp"

script :: Array Prop -> Markup -> Markup
script = element Nothing "script"

section :: Array Prop -> Markup -> Markup
section = element Nothing "section"

select :: Array Prop -> Markup -> Markup
select = element Nothing "select"

small :: Array Prop -> Markup -> Markup
small = element Nothing "small"

source :: Array Prop -> Markup -> Markup
source = element Nothing "source"

span :: Array Prop -> Markup -> Markup
span = element Nothing "span"

strong :: Array Prop -> Markup -> Markup
strong = element Nothing "strong"

style :: Array Prop -> Markup -> Markup
style = element Nothing "style"

sub :: Array Prop -> Markup -> Markup
sub = element Nothing "sub"

summary :: Array Prop -> Markup -> Markup
summary = element Nothing "summary"

sup :: Array Prop -> Markup -> Markup
sup = element Nothing "sup"

table :: Array Prop -> Markup -> Markup
table = element Nothing "table"

tbody :: Array Prop -> Markup -> Markup
tbody = element Nothing "tbody"

td :: Array Prop -> Markup -> Markup
td = element Nothing "td"

template :: Array Prop -> Markup -> Markup
template = element Nothing "template"

textarea :: Array Prop -> Markup -> Markup
textarea = element Nothing "textarea"

tfoot :: Array Prop -> Markup -> Markup
tfoot = element Nothing "tfoot"

th :: Array Prop -> Markup -> Markup
th = element Nothing "th"

thead :: Array Prop -> Markup -> Markup
thead = element Nothing "thead"

time :: Array Prop -> Markup -> Markup
time = element Nothing "time"

title :: Array Prop -> Markup -> Markup
title = element Nothing "title"

tr :: Array Prop -> Markup -> Markup
tr = element Nothing "tr"

track :: Array Prop -> Markup -> Markup
track = element Nothing "track"

u :: Array Prop -> Markup -> Markup
u = element Nothing "u"

ul :: Array Prop -> Markup -> Markup
ul = element Nothing "ul"

ul_ :: Markup -> Markup
ul_ = element Nothing "ul" []

var :: Array Prop -> Markup -> Markup
var = element Nothing "var"

video :: Array Prop -> Markup -> Markup
video = element Nothing "video"

wbr :: Array Prop -> Markup -> Markup
wbr = element Nothing "wbr"

--------------------------------------------------------------------------------
-- Attributes

textA :: String -> Prop
textA = attr "text"

titleA :: String -> Prop
titleA = attr "title"

classA :: String -> Prop
classA = attr "class"

typeA :: String -> Prop
typeA = attr "type"

placeholderA :: String -> Prop
placeholderA = attr "placeholder"

srcA :: String -> Prop
srcA = attr "src"

heightA :: Int -> Prop
heightA px = attr "height" $ show px

widthA :: Int -> Prop
widthA px = attr "width" $ show px

styleA :: String -> Prop
styleA = attr "style"

valueA :: String -> Prop
valueA = prop "value"

checkedA :: Boolean -> Prop
checkedA = prop "checked"

--------------------------------------------------------------------------------
-- Event Handlers

onClick :: forall eff. (Event () -> Eff eff Unit) -> Prop
onClick h = handle "click" h

onInput :: forall eff a. (IsForeign a) => (Event () -> Maybe a -> Eff eff Unit) -> Prop
onInput h = handle "input" $ \e -> h e (getProp "value" e)

onKeydown :: forall eff a. (IsForeign a) => (KeyboardEvent () -> Maybe a -> Eff eff Unit) -> Prop
onKeydown h = handle "keydown" $ \e -> h e (getProp "value" e)

onChecked :: forall eff a. (Event () -> Boolean -> Eff eff Unit) -> Prop
onChecked h = handle "change" $ \e -> h e (maybe false id $ getProp "checked" e)

onInitialized :: forall eff. UniqueStr -> (HTMLElement -> Eff eff Unit) -> Prop
onInitialized s = initializer ("opticui-init-" ++ s)

onFinalized :: forall eff. UniqueStr -> (HTMLElement -> Eff eff Unit) -> Prop
onFinalized s = finalizer ("opticui-fin-" ++ s)

getProp :: forall a r. (IsForeign a) => String -> Event r -> Maybe a
getProp p = either (const Nothing) Just <<< readProp p <<< toForeign <<< _.target
