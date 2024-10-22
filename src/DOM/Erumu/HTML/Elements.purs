module DOM.Erumu.HTML.Elements
  ( div_
  , span
  , br
  , hr
  , a
  , i
  , strong
  , code
  , h1
  , h2
  , h3
  , h4
  , h5
  , p

  , dt
  , dd
  , dl
  , footer

  , ol
  , ul
  , li
  , section
  , table
  , thead
  , tbody
  , tr
  , td
  , th

  , iframe
  , object
  , embed
  , img

  , nav
  , main
  , header
  , aside
  , legend

  , label
  , form
  , input
  , textArea
  , select
  , option
  , button
  , time
  , em
  , fieldset

  , address
  , script
  , noscript

  , svg
  , path
  , rect
  , graphic
  , polygon
  , circle
  , line
  , linearGradient
  , stop
  , text

  , ElementFn
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

iframe :: forall msg. ElementFn msg
iframe = element "iframe"

object :: forall msg. ElementFn msg
object = element "object"

embed :: forall msg. ElementFn msg
embed = element "embed"

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

option :: forall msg. ElementFn msg
option = element "option"

fieldset :: forall msg. ElementFn msg
fieldset = element "fieldset"

button :: forall msg. ElementFn msg
button = element "button"

time :: forall msg. ElementFn msg
time = element "time"

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