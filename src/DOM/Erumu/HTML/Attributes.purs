module DOM.Erumu.HTML.Attributes
  ( action
  , alt
  , ariaControls
  , ariaExpanded
  , ariaHaspopup
  , ariaHidden
  , ariaLabelledby
  , ariaModal
  , ariaOrientation
  , autocomplete
  , autofocus
  , cRadius
  , checked
  , classN_
  , class_
  , classes
  , clipRule
  , colSpan
  , cxCoord
  , cyCoord
  , data_
  , defaultValue
  , defer
  , dims
  , disabled
  , enctype
  , fill
  , fillRule
  , fontFamily
  , fontSize
  , for
  , gradientUnits
  , height
  , href
  , id_
  , method
  , name
  , noop
  , offset
  , placeholder
  , points
  , role
  , rows
  , rx
  , selected
  , src
  , stroke
  , strokeLinecap
  , strokeLinejoin
  , strokeWidth
  , style
  , tabindex
  , target
  , title
  , transform
  , type_
  , value
  , viewBox
  , width
  , x1Coord
  , x2Coord
  , xCoord
  , xmlns
  , y1Coord
  , y2Coord
  , yCoord
  )
  where

import Prelude
import Data.String (joinWith)

import DOM.Erumu.Types (attribute, Prop)

action :: forall msg. String -> Prop msg
action = attribute "action"

alt :: forall msg. String -> Prop msg
alt = attribute "alt"

ariaControls :: forall msg. String -> Prop msg
ariaControls = attribute "aria-controls"

ariaExpanded :: forall msg. Boolean -> Prop msg
ariaExpanded = attribute "aria-expanded" <<< show

ariaHaspopup :: forall msg. Boolean -> Prop msg
ariaHaspopup = attribute "aria-haspopup" <<< show

ariaHidden :: forall msg. Boolean -> Prop msg
ariaHidden = attribute "aria-hidden" <<< show

ariaLabelledby :: forall msg. String -> Prop msg
ariaLabelledby = attribute "aria-labelledby"

ariaModal :: forall msg. Boolean -> Prop msg
ariaModal = attribute "aria-modal" <<< show

ariaOrientation :: forall msg. String -> Prop msg
ariaOrientation = attribute "aria-orientation"

autocomplete :: forall msg. String -> Prop msg
autocomplete = attribute "autocomplete"

autofocus :: forall msg. String -> Prop msg
autofocus = attribute "autofocus"

checked :: forall msg. String -> Prop msg
checked = attribute "checked"

-- Use this attribute for normal elements
class_ :: forall msg. String -> Prop msg
class_ = attribute "className"

classes :: forall msg. Array String -> Prop msg
classes = class_ <<< joinWith " "

-- Use this attribute for SVG (and other namespaced) elements.
classN_ :: forall msg. String -> Prop msg
classN_ = attribute "class"

clipRule :: forall msg. String -> Prop msg
clipRule = attribute "clip-rule"

colSpan :: forall msg. Int -> Prop msg
colSpan = attribute "colSpan" <<< show

cRadius :: forall msg. String -> Prop msg
cRadius = attribute "r"

cxCoord :: forall msg. String -> Prop msg
cxCoord = attribute "cx"

cyCoord :: forall msg. String -> Prop msg
cyCoord = attribute "cy"

data_ :: forall msg. String -> Prop msg
data_ = attribute "data"

defaultValue :: forall msg. String -> Prop msg
defaultValue = attribute "defaultValue"

defer :: forall msg. String -> Prop msg
defer = attribute "defer"

dims :: forall msg. String -> Prop msg
dims = attribute "d"

disabled :: forall msg. String -> Prop msg
disabled = attribute "disabled"

enctype :: forall msg. String -> Prop msg
enctype = attribute "enctype"

fill :: forall msg. String -> Prop msg
fill = attribute "fill"

fillRule :: forall msg. String -> Prop msg
fillRule = attribute "fill-rule"

fontFamily :: forall msg. String -> Prop msg
fontFamily = attribute "font-family"

fontSize :: forall msg. String -> Prop msg
fontSize = attribute "font-size"

for :: forall msg. String -> Prop msg
for = attribute "for"

gradientUnits :: forall msg. String -> Prop msg
gradientUnits = attribute "gradientUnits"

height :: forall msg. String -> Prop msg
height = attribute "height"

href :: forall msg. String -> Prop msg
href = attribute "href"

id_ :: forall msg. String -> Prop msg
id_ = attribute "id"

method :: forall msg. String -> Prop msg
method = attribute "method"

name :: forall msg. String -> Prop msg
name = attribute "name"

-- "no-op" Prop renders no attribute
noop :: forall msg. Prop msg
noop = attribute "" ""

offset :: forall msg. String -> Prop msg
offset = attribute "offset"

placeholder :: forall msg. String -> Prop msg
placeholder = attribute "placeholder"

points :: forall msg. String -> Prop msg
points = attribute "points"

role :: forall msg. String -> Prop msg
role = attribute "role"

rows :: forall msg. Int -> Prop msg
rows = attribute "rows" <<< show

rx :: forall msg. String -> Prop msg
rx = attribute "rx"

selected :: forall msg. String -> Prop msg
selected = attribute "selected"

src :: forall msg. String -> Prop msg
src = attribute "src"

stroke :: forall msg. String -> Prop msg
stroke = attribute "stroke"

strokeLinecap :: forall msg. String -> Prop msg
strokeLinecap = attribute "stroke-linecap"

strokeLinejoin :: forall msg. String -> Prop msg
strokeLinejoin = attribute "stroke-linejoin"

strokeWidth :: forall msg. String -> Prop msg
strokeWidth = attribute "stroke-width"

style :: forall msg. String -> Prop msg
style = attribute "style"

tabindex :: forall msg. String -> Prop msg
tabindex = attribute "tabindex"

target :: forall msg. String -> Prop msg
target = attribute "target"

title :: forall msg. String -> Prop msg
title = attribute "title"

transform :: forall msg. String -> Prop msg
transform = attribute "transform"

type_ :: forall msg. String -> Prop msg
type_ = attribute "type"

value :: forall msg. String -> Prop msg
value = attribute "value"

viewBox :: forall msg. String -> Prop msg
viewBox = attribute "viewBox"

width :: forall msg. String -> Prop msg
width = attribute "width"

x1Coord :: forall msg. String -> Prop msg
x1Coord = attribute "x1"

x2Coord :: forall msg. String -> Prop msg
x2Coord = attribute "x2"

xCoord :: forall msg. String -> Prop msg
xCoord = attribute "x"

xmlns :: forall msg. String -> Prop msg
xmlns = attribute "namespace"

y1Coord :: forall msg. String -> Prop msg
y1Coord = attribute "y1"

y2Coord :: forall msg. String -> Prop msg
y2Coord = attribute "y2"

yCoord :: forall msg. String -> Prop msg
yCoord = attribute "y"
