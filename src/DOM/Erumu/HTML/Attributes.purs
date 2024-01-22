module DOM.Erumu.HTML.Attributes
  ( class_, classes, classN_, id_
  , href, src, target
  , title, alt, for
  , type_, value, selected, checked, autocomplete, placeholder, disabled, defaultValue, tabindex
  , autofocus
  , rows, style
  , colSpan
  , action, method, enctype, name
  , role
  , width, height
  , defer
  , data_, dataLegId, dataClipboardText, dataPopupName, dataPopupOpens
  , noop
  , fill, viewBox, strokeWidth, stroke, dims, strokeLinecap, strokeLinejoin, xmlns, clipRule, fillRule
  , rx, xCoord, yCoord, x1Coord, y1Coord, x2Coord, y2Coord, points, transform
  , cxCoord, cyCoord, cRadius, gradientUnits, offset
  , fontFamily, fontSize
  ) where

import Prelude

import Data.String (joinWith)

import DOM.Erumu.Types (attribute, Prop)

-- "no-op" Prop renders no attribute
noop :: forall msg. Prop msg
noop = attribute "" ""

role :: forall msg. String -> Prop msg
role = attribute "role"

action :: forall msg. String -> Prop msg
action = attribute "action"

method :: forall msg. String -> Prop msg
method = attribute "method"

enctype :: forall msg. String -> Prop msg
enctype = attribute "enctype"

name :: forall msg. String -> Prop msg
name = attribute "name"

-- Use this attribute for normal elements
class_ :: forall msg. String -> Prop msg
class_ = attribute "className"

-- Use this attribute for SVG (and other namespaced) elements.
classN_ :: forall msg. String -> Prop msg
classN_ = attribute "class"

classes :: forall msg. Array String -> Prop msg
classes = class_ <<< joinWith " "

id_ :: forall msg. String -> Prop msg
id_ = attribute "id"

href :: forall msg. String -> Prop msg
href = attribute "href"

target :: forall msg. String -> Prop msg
target = attribute "target"

src :: forall msg. String -> Prop msg
src = attribute "src"

title :: forall msg. String -> Prop msg
title = attribute "title"

alt :: forall msg. String -> Prop msg
alt = attribute "alt"

for :: forall msg. String -> Prop msg
for = attribute "for"

type_ :: forall msg. String -> Prop msg
type_ = attribute "type"

value :: forall msg. String -> Prop msg
value = attribute "value"

defaultValue :: forall msg. String -> Prop msg
defaultValue = attribute "defaultValue"

selected :: forall msg. String -> Prop msg
selected = attribute "selected"

checked :: forall msg. String -> Prop msg
checked = attribute "checked"

tabindex :: forall msg. String -> Prop msg
tabindex = attribute "tabindex"

autocomplete :: forall msg. String -> Prop msg
autocomplete = attribute "autocomplete"

placeholder :: forall msg. String -> Prop msg
placeholder = attribute "placeholder"

disabled :: forall msg. String -> Prop msg
disabled = attribute "disabled"

autofocus :: forall msg. String -> Prop msg
autofocus = attribute "autofocus"

rows :: forall msg. Int -> Prop msg
rows = attribute "rows" <<< show

style :: forall msg. String -> Prop msg
style = attribute "style"

colSpan :: forall msg. Int -> Prop msg
colSpan = attribute "colSpan" <<< show

width :: forall msg. String -> Prop msg
width = attribute "width"

height :: forall msg. String -> Prop msg
height = attribute "height"

defer :: forall msg. String -> Prop msg
defer = attribute "defer"

data_ :: forall msg. String -> Prop msg
data_ = attribute "data"

dataClipboardText :: forall msg. String -> Prop msg
dataClipboardText = attribute "data-clipboard-text"

dataPopupName :: forall msg. String -> Prop msg
dataPopupName = attribute "data-popup-name"

dataPopupOpens :: forall msg. String -> Prop msg
dataPopupOpens = attribute "data-popup-opens"

dataLegId :: forall msg. String -> Prop msg
dataLegId = attribute "data-leg-id"

fill :: forall msg. String -> Prop msg
fill = attribute "fill"

viewBox :: forall msg. String -> Prop msg
viewBox = attribute "viewBox"

strokeWidth :: forall msg. String -> Prop msg
strokeWidth = attribute "stroke-width"

stroke :: forall msg. String -> Prop msg
stroke = attribute "stroke"

clipRule :: forall msg. String -> Prop msg
clipRule = attribute "clip-rule"

fillRule :: forall msg. String -> Prop msg
fillRule = attribute "fill-rule"

rx :: forall msg. String -> Prop msg
rx = attribute "rx"

xCoord :: forall msg. String -> Prop msg
xCoord = attribute "x"

yCoord :: forall msg. String -> Prop msg
yCoord = attribute "y"

x1Coord :: forall msg. String -> Prop msg
x1Coord = attribute "x1"

y1Coord :: forall msg. String -> Prop msg
y1Coord = attribute "y1"

x2Coord :: forall msg. String -> Prop msg
x2Coord = attribute "x2"

y2Coord :: forall msg. String -> Prop msg
y2Coord = attribute "y2"

cxCoord :: forall msg. String -> Prop msg
cxCoord = attribute "cx"

cyCoord :: forall msg. String -> Prop msg
cyCoord = attribute "cy"

cRadius :: forall msg. String -> Prop msg
cRadius = attribute "r"

points :: forall msg. String -> Prop msg
points = attribute "points"

gradientUnits :: forall msg. String -> Prop msg
gradientUnits = attribute "gradientUnits"

offset :: forall msg. String -> Prop msg
offset = attribute "offset"

dims :: forall msg. String -> Prop msg
dims = attribute "d"

strokeLinecap :: forall msg. String -> Prop msg
strokeLinecap = attribute "stroke-linecap"

strokeLinejoin :: forall msg. String -> Prop msg
strokeLinejoin = attribute "stroke-linejoin"

transform :: forall msg. String -> Prop msg
transform = attribute "transform"

xmlns :: forall msg. String -> Prop msg
xmlns = attribute "namespace"

fontFamily :: forall msg. String -> Prop msg
fontFamily = attribute "font-family"

fontSize :: forall msg. String -> Prop msg
fontSize = attribute "font-size"
