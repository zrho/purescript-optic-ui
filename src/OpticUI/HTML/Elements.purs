module OpticUI.HTML.Elements where
--------------------------------------------------------------------------------
import Prelude hiding (sub, div, map)
import OpticUI.HTML (HTML (..), Prop ())
import Data.Maybe (Maybe (Nothing))
--------------------------------------------------------------------------------

text :: forall eff. String -> HTML eff
text = Text

a :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
a = Element Nothing "a"

abbr :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
abbr = Element Nothing "abbr"

address :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
address = Element Nothing "address"

area :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
area = Element Nothing "area"

article :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
article = Element Nothing "article"

aside :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
aside = Element Nothing "aside"

audio :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
audio = Element Nothing "audio"

b :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
b = Element Nothing "b"

base :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
base = Element Nothing "base"

bdi :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
bdi = Element Nothing "bdi"

bdo :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
bdo = Element Nothing "bdo"

blockquote :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
blockquote = Element Nothing "blockquote"

body :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
body = Element Nothing "body"

br :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
br = Element Nothing "br"

button :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
button = Element Nothing "button"

canvas :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
canvas = Element Nothing "canvas"

caption :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
caption = Element Nothing "caption"

cite :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
cite = Element Nothing "cite"

code :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
code = Element Nothing "code"

col :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
col = Element Nothing "col"

colgroup :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
colgroup = Element Nothing "colgroup"

dataTag :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
dataTag = Element Nothing "data"

datalist :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
datalist = Element Nothing "datalist"

dd :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
dd = Element Nothing "dd"

del :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
del = Element Nothing "del"

details :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
details = Element Nothing "details"

dfn :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
dfn = Element Nothing "dfn"

dialog :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
dialog = Element Nothing "dialog"

div :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
div = Element Nothing "div"

div_ :: forall m. Array (HTML m) -> HTML m
div_ = Element Nothing "div" []

dl :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
dl = Element Nothing "dl"

dt :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
dt = Element Nothing "dt"

em :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
em = Element Nothing "em"

embed :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
embed = Element Nothing "embed"

fieldset :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
fieldset = Element Nothing "fieldset"

figcaption :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
figcaption = Element Nothing "figcaption"

figure :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
figure = Element Nothing "figure"

footer :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
footer = Element Nothing "footer"

form :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
form = Element Nothing "form"

h1 :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
h1 = Element Nothing "h1"

h2 :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
h2 = Element Nothing "h2"

h3 :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
h3 = Element Nothing "h3"

h4 :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
h4 = Element Nothing "h4"

h5 :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
h5 = Element Nothing "h5"

h6 :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
h6 = Element Nothing "h6"

h1_ :: forall m. Array (HTML m) -> HTML m
h1_ = Element Nothing "h1" []

h2_ :: forall m. Array (HTML m) -> HTML m
h2_ = Element Nothing "h2" []

h3_ :: forall m. Array (HTML m) -> HTML m
h3_ = Element Nothing "h3" []

h4_ :: forall m. Array (HTML m) -> HTML m
h4_ = Element Nothing "h4" []

h5_ :: forall m. Array (HTML m) -> HTML m
h5_ = Element Nothing "h5" []

h6_ :: forall m. Array (HTML m) -> HTML m
h6_ = Element Nothing "h6" []

head :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
head = Element Nothing "head"

header :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
header = Element Nothing "header"

hgroup :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
hgroup = Element Nothing "hgroup"

hr :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
hr = Element Nothing "hr"

html :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
html = Element Nothing "html"

i :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
i = Element Nothing "i"

iframe :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
iframe = Element Nothing "iframe"

img :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
img = Element Nothing "img"

input :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
input = Element Nothing "input"

input_ :: forall m. Array (Prop m) -> HTML m
input_ ps = Element Nothing "input" ps []

ins :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
ins = Element Nothing "ins"

kbd :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
kbd = Element Nothing "kbd"

keygen :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
keygen = Element Nothing "keygen"

label :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
label = Element Nothing "label"

legend :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
legend = Element Nothing "legend"

li :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
li = Element Nothing "li"

li_ :: forall m. Array (HTML m) -> HTML m
li_ = Element Nothing "li" []

link :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
link = Element Nothing "link"

main :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
main = Element Nothing "main"

map :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
map = Element Nothing "map"

mark :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
mark = Element Nothing "mark"

menu :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
menu = Element Nothing "menu"

menuitem :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
menuitem = Element Nothing "menuitem"

meta :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
meta = Element Nothing "meta"

meter :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
meter = Element Nothing "meter"

nav :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
nav = Element Nothing "nav"

noscript :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
noscript = Element Nothing "noscript"

object :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
object = Element Nothing "object"

ol :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
ol = Element Nothing "ol"

optgroup :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
optgroup = Element Nothing "optgroup"

option :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
option = Element Nothing "option"

output :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
output = Element Nothing "output"

p :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
p = Element Nothing "p"

p_ :: forall m. Array (HTML m) -> HTML m
p_ = Element Nothing "p" []

param :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
param = Element Nothing "param"

pre :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
pre = Element Nothing "pre"

progress :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
progress = Element Nothing "progress"

q :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
q = Element Nothing "q"

rb :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
rb = Element Nothing "rb"

rp :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
rp = Element Nothing "rp"

rt :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
rt = Element Nothing "rt"

rtc :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
rtc = Element Nothing "rtc"

ruby :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
ruby = Element Nothing "ruby"

s :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
s = Element Nothing "s"

samp :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
samp = Element Nothing "samp"

script :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
script = Element Nothing "script"

section :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
section = Element Nothing "section"

select :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
select = Element Nothing "select"

small :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
small = Element Nothing "small"

source :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
source = Element Nothing "source"

span :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
span = Element Nothing "span"

strong :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
strong = Element Nothing "strong"

style :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
style = Element Nothing "style"

sub :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
sub = Element Nothing "sub"

summary :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
summary = Element Nothing "summary"

sup :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
sup = Element Nothing "sup"

table :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
table = Element Nothing "table"

tbody :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
tbody = Element Nothing "tbody"

td :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
td = Element Nothing "td"

template :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
template = Element Nothing "template"

textarea :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
textarea = Element Nothing "textarea"

tfoot :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
tfoot = Element Nothing "tfoot"

th :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
th = Element Nothing "th"

thead :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
thead = Element Nothing "thead"

time :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
time = Element Nothing "time"

title :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
title = Element Nothing "title"

tr :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
tr = Element Nothing "tr"

track :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
track = Element Nothing "track"

u :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
u = Element Nothing "u"

ul :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
ul = Element Nothing "ul"

ul_ :: forall m. Array (HTML m) -> HTML m
ul_ = Element Nothing "ul" []

var :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
var = Element Nothing "var"

video :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
video = Element Nothing "video"

wbr :: forall m. Array (Prop m) -> Array (HTML m) -> HTML m
wbr = Element Nothing "wbr"
