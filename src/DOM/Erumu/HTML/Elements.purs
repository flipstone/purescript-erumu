module DOM.Erumu.HTML.Elements
  ( ElementFn
  , a
  , abbr
  , address
  , area
  , article
  , aside
  , audio
  , b
  , base
  , bdi
  , bdo
  , blockquote
  , body
  , br
  , button
  , canvas
  , caption
  , circle
  , cite
  , code
  , col
  , colgroup
  , data_
  , datalist
  , dd
  , defs
  , del
  , details
  , dfn
  , dialog
  , div_
  , dl
  , dt
  , em
  , embed
  , fieldset
  , figcaption
  , figure
  , footer
  , form
  , graphic
  , h1
  , h2
  , h3
  , h4
  , h5
  , head
  , header
  , hgroup
  , hr
  , html
  , i
  , iframe
  , img
  , input
  , ins
  , kbd
  , label
  , legend
  , li
  , line
  , linearGradient
  , main
  , map
  , mark
  , menu
  , meta
  , meter
  , nav
  , noscript
  , object
  , ol
  , optgroup
  , option
  , output
  , p
  , path
  , picture
  , polygon
  , pre
  , progress
  , q
  , rect
  , rp
  , rt
  , ruby
  , s
  , samp
  , script
  , search
  , section
  , select
  , slot
  , small
  , source
  , span
  , stop
  , strong
  , sub
  , summary
  , sup
  , svg
  , table
  , tbody
  , td
  , template
  , text
  , textArea
  , th
  , thead
  , time
  , tr
  , track
  , u
  , ul
  , var
  , video
  , wbr
  ) where

import Prelude

import DOM.Erumu.Types (element, attribute, HTML, Prop)

type ElementFn msg = Array (Prop msg) -> Array (HTML msg) -> HTML msg

svgNamespaceProp :: forall msg. Prop msg
svgNamespaceProp = attribute "namespace" "http://www.w3.org/2000/svg"

appendSvgNs :: forall msg. Array (Prop msg) -> Array (Prop msg)
appendSvgNs props = [ svgNamespaceProp ] <> props

svgNamespacedElement :: forall msg. String -> ElementFn msg
svgNamespacedElement tagName props =
  element tagName (appendSvgNs props)

div_ :: forall msg. ElementFn msg
div_ = element "div"

header :: forall msg. ElementFn msg
header = element "header"

aside :: forall msg. ElementFn msg
aside = element "aside"

legend :: forall msg. ElementFn msg
legend = element "legend"

span :: forall msg. ElementFn msg
span = element "span"

hr :: forall msg. ElementFn msg
hr = element "hr"

br :: forall msg. ElementFn msg
br = element "br"

i :: forall msg. ElementFn msg
i = element "i"

strong :: forall msg. ElementFn msg
strong = element "strong"

code :: forall msg. ElementFn msg
code = element "code"

a :: forall msg. ElementFn msg
a = element "a"

abbr :: forall msg. ElementFn msg
abbr = element "abbr"

area :: forall msg. ElementFn msg
area = element "area"

article :: forall msg. ElementFn msg
article = element "article"

audio :: forall msg. ElementFn msg
audio = element "audio"

b :: forall msg. ElementFn msg
b = element "b"

base :: forall msg. ElementFn msg
base = element "base"

bdi :: forall msg. ElementFn msg
bdi = element "bdi"

bdo :: forall msg. ElementFn msg
bdo = element "bdo"

blockquote :: forall msg. ElementFn msg
blockquote = element "blockquote"

body :: forall msg. ElementFn msg
body = element "body"

canvas :: forall msg. ElementFn msg
canvas = element "canvas"

caption :: forall msg. ElementFn msg
caption = element "caption"

cite :: forall msg. ElementFn msg
cite = element "cite"

col :: forall msg. ElementFn msg
col = element "col"

colgroup :: forall msg. ElementFn msg
colgroup = element "colgroup"

data_ :: forall msg. ElementFn msg
data_ = element "data"

datalist :: forall msg. ElementFn msg
datalist = element "datalist"

del :: forall msg. ElementFn msg
del = element "del"

details :: forall msg. ElementFn msg
details = element "details"

dfn :: forall msg. ElementFn msg
dfn = element "dfn"

dialog :: forall msg. ElementFn msg
dialog = element "dialog"

ins :: forall msg. ElementFn msg
ins = element "ins"

iframe :: forall msg. ElementFn msg
iframe = element "iframe"

object :: forall msg. ElementFn msg
object = element "object"

embed :: forall msg. ElementFn msg
embed = element "embed"

figcaption :: forall msg. ElementFn msg
figcaption = element "figcaption"

figure :: forall msg. ElementFn msg
figure = element "figure"

img :: forall msg. ElementFn msg
img = element "img"

h1 :: forall msg. ElementFn msg
h1 = element "h1"

h2 :: forall msg. ElementFn msg
h2 = element "h2"

h3 :: forall msg. ElementFn msg
h3 = element "h3"

h4 :: forall msg. ElementFn msg
h4 = element "h4"

h5 :: forall msg. ElementFn msg
h5 = element "h5"

head :: forall msg. ElementFn msg
head = element "head"

hgroup :: forall msg. ElementFn msg
hgroup = element "hgroup"

html :: forall msg. ElementFn msg
html = element "html"

kbd :: forall msg. ElementFn msg
kbd = element "kbd"

mark :: forall msg. ElementFn msg
mark = element "mark"

menu :: forall msg. ElementFn msg
menu = element "menu"

meta :: forall msg. ElementFn msg
meta = element "meta"

meter :: forall msg. ElementFn msg
meter = element "meter"

p :: forall msg. ElementFn msg
p = element "p"

dl :: forall msg. ElementFn msg
dl = element "dl"

dt :: forall msg. ElementFn msg
dt = element "dt"

dd :: forall msg. ElementFn msg
dd = element "dd"

footer :: forall msg. ElementFn msg
footer = element "footer"

ol :: forall msg. ElementFn msg
ol = element "ol"

ul :: forall msg. ElementFn msg
ul = element "ul"

li :: forall msg. ElementFn msg
li = element "li"

section :: forall msg. ElementFn msg
section = element "section"

table :: forall msg. ElementFn msg
table = element "table"

tbody :: forall msg. ElementFn msg
tbody = element "tbody"

thead :: forall msg. ElementFn msg
thead = element "thead"

tr :: forall msg. ElementFn msg
tr = element "tr"

th :: forall msg. ElementFn msg
th = element "th"

td :: forall msg. ElementFn msg
td = element "td"

nav :: forall msg. ElementFn msg
nav = element "nav"

label :: forall msg. ElementFn msg
label = element "label"

form :: forall msg. ElementFn msg
form = element "form"

input :: forall msg. ElementFn msg
input = element "input"

textArea :: forall msg. ElementFn msg
textArea = element "textarea"

select :: forall msg. ElementFn msg
select = element "select"

optgroup :: forall msg. ElementFn msg
optgroup = element "optgroup"

option :: forall msg. ElementFn msg
option = element "option"

output :: forall msg. ElementFn msg
output = element "output"

picture :: forall msg. ElementFn msg
picture = element "picture"

pre :: forall msg. ElementFn msg
pre = element "pre"

progress :: forall msg. ElementFn msg
progress = element "progress"

q :: forall msg. ElementFn msg
q = element "q"

rp :: forall msg. ElementFn msg
rp = element "rp"

rt :: forall msg. ElementFn msg
rt = element "rt"

ruby :: forall msg. ElementFn msg
ruby = element "ruby"

s :: forall msg. ElementFn msg
s = element "s"

samp :: forall msg. ElementFn msg
samp = element "samp"

search :: forall msg. ElementFn msg
search = element "search"

slot :: forall msg. ElementFn msg
slot = element "slot"

small :: forall msg. ElementFn msg
small = element "small"

source :: forall msg. ElementFn msg
source = element "source"

sub :: forall msg. ElementFn msg
sub = element "sub"

summary :: forall msg. ElementFn msg
summary = element "summary"

sup :: forall msg. ElementFn msg
sup = element "sup"

template :: forall msg. ElementFn msg
template = element "template"

fieldset :: forall msg. ElementFn msg
fieldset = element "fieldset"

button :: forall msg. ElementFn msg
button = element "button"

time :: forall msg. ElementFn msg
time = element "time"

track :: forall msg. ElementFn msg
track = element "track"

u :: forall msg. ElementFn msg
u = element "u"

var :: forall msg. ElementFn msg
var = element "var"

video :: forall msg. ElementFn msg
video = element "video"

wbr :: forall msg. ElementFn msg
wbr = element "wbr"

em :: forall msg. ElementFn msg
em = element "em"

address :: forall msg. ElementFn msg
address = element "address"

script :: forall msg. ElementFn msg
script = element "script"

noscript :: forall msg. ElementFn msg
noscript = element "noscript"

main :: forall msg. ElementFn msg
main = element "main"

map :: forall msg. ElementFn msg
map = element "map"

svg :: forall msg. ElementFn msg
svg = svgNamespacedElement "svg"

path :: forall msg. ElementFn msg
path = svgNamespacedElement "path"

rect :: forall msg. ElementFn msg
rect = svgNamespacedElement "rect"

line :: forall msg. ElementFn msg
line = svgNamespacedElement "line"

circle :: forall msg. ElementFn msg
circle = svgNamespacedElement "circle"

graphic :: forall msg. ElementFn msg
graphic = svgNamespacedElement "g"

polygon :: forall msg. ElementFn msg
polygon = svgNamespacedElement "polygon"

linearGradient :: forall msg. ElementFn msg
linearGradient = svgNamespacedElement "linearGradient"

stop :: forall msg. ElementFn msg
stop = svgNamespacedElement "stop"

text :: forall msg. ElementFn msg
text = svgNamespacedElement "text"

defs :: forall msg. ElementFn msg
defs = svgNamespacedElement "defs"