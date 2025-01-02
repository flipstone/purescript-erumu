module DOM.Erumu.HTML.Attributes
  ( accept
  , action
  , alt
  , ariaActivedescendant
  , ariaAtomic
  , ariaAutocomplete
  , ariaBraillelabel
  , ariaBrailleroledescription
  , ariaBusy
  , ariaChecked
  , ariaColcount
  , ariaColindex
  , ariaColindextext
  , ariaColspan
  , ariaControls
  , ariaCurrent
  , ariaDescribedby
  , ariaDescription
  , ariaDetails
  , ariaDisabled
  , ariaErrormessage
  , ariaExpanded
  , ariaFlowto
  , ariaHaspopup
  , ariaHidden
  , ariaInvalid
  , ariaKeyshortcuts
  , ariaLabel
  , ariaLabelledby
  , ariaLevel
  , ariaLive
  , ariaModal
  , ariaMultiline
  , ariaMultiselectable
  , ariaOrientation
  , ariaOwns
  , ariaPlaceholder
  , ariaPosinset
  , ariaPressed
  , ariaReadonly
  , ariaRelevant
  , ariaRequired
  , ariaRoledescription
  , ariaRowcount
  , ariaRowindex
  , ariaRowindextext
  , ariaRowspan
  , ariaSelected
  , ariaSetsize
  , ariaSort
  , ariaValuemax
  , ariaValuemin
  , ariaValuenow
  , ariaValuetext
  , autocomplete
  , autofocus
  , cRadius
  , checked
  , classN_
  , class_
  , classes
  , clipRule
  , colSpan
  , contenteditable
  , crossorigin
  , cxCoord
  , cyCoord
  , dataCustom
  , data_
  , defaultValue
  , defer
  , dims
  , dir
  , dirname
  , disabled
  , draggable
  , elementtiming
  , enctype
  , enterkeyhint
  , exportparts
  , fill
  , fillRule
  , fontFamily
  , fontSize
  , for
  , gradientUnits
  , height
  , hidden
  , href
  , id_
  , inert
  , inputmode
  , itemid
  , itemprop
  , itemref
  , itemscope
  , itemtype
  , lang
  , max
  , maxlength
  , method
  , min
  , minlength
  , multiple
  , name
  , nonce
  , noop
  , offset
  , part
  , pattern
  , placeholder
  , points
  , popover
  , popovertarget
  , readonly
  , required
  , role
  , rows
  , rx
  , selected
  , size
  , slot
  , spellcheck
  , src
  , step
  , stroke
  , strokeLinecap
  , strokeLinejoin
  , strokeWidth
  , style
  , tabindex
  , target
  , title
  , transform
  , translate
  , type_
  , value
  , viewBox
  , width
  , writingsuggestions
  , x1Coord
  , x2Coord
  , xCoord
  , xmlns
  , y1Coord
  , y2Coord
  , yCoord
  ) where

import Prelude

import DOM.Erumu.Types (attribute, Prop)
import Data.String (joinWith)

action :: forall msg. String -> Prop msg
action = attribute "action"

alt :: forall msg. String -> Prop msg
alt = attribute "alt"

-- ARIA attributes

ariaAttribute :: forall msg. String -> String -> Prop msg
ariaAttribute key ariaValue =
  attribute ("aria-" <> key) ariaValue

ariaActivedescendant :: forall msg. String -> Prop msg
ariaActivedescendant = ariaAttribute "activedescendant"

ariaAtomic :: forall msg. Boolean -> Prop msg
ariaAtomic = ariaAttribute "atomic" <<< show

ariaAutocomplete :: forall msg. String -> Prop msg
ariaAutocomplete = ariaAttribute "autocomplete"

ariaBraillelabel :: forall msg. String -> Prop msg
ariaBraillelabel = ariaAttribute "braillelabel"

ariaBrailleroledescription :: forall msg. String -> Prop msg
ariaBrailleroledescription = ariaAttribute "brailleroledescription"

ariaBusy :: forall msg. Boolean -> Prop msg
ariaBusy = ariaAttribute "busy" <<< show

ariaChecked :: forall msg. Boolean -> Prop msg
ariaChecked = ariaAttribute "checked" <<< show

ariaColcount :: forall msg. Int -> Prop msg
ariaColcount = ariaAttribute "colcount" <<< show

ariaColindex :: forall msg. Int -> Prop msg
ariaColindex = ariaAttribute "colindex" <<< show

ariaColindextext :: forall msg. String -> Prop msg
ariaColindextext = ariaAttribute "colindextext"

ariaColspan :: forall msg. Int -> Prop msg
ariaColspan = ariaAttribute "colspan" <<< show

ariaControls :: forall msg. String -> Prop msg
ariaControls = ariaAttribute "controls"

ariaCurrent :: forall msg. String -> Prop msg
ariaCurrent = ariaAttribute "current"

ariaDescribedby :: forall msg. String -> Prop msg
ariaDescribedby = ariaAttribute "describedby"

ariaDescription :: forall msg. String -> Prop msg
ariaDescription = ariaAttribute "description"

ariaDetails :: forall msg. String -> Prop msg
ariaDetails = ariaAttribute "details"

ariaDisabled :: forall msg. Boolean -> Prop msg
ariaDisabled = ariaAttribute "disabled" <<< show

ariaErrormessage :: forall msg. String -> Prop msg
ariaErrormessage = ariaAttribute "errormessage"

ariaExpanded :: forall msg. Boolean -> Prop msg
ariaExpanded = ariaAttribute "expanded" <<< show

ariaFlowto :: forall msg. String -> Prop msg
ariaFlowto = ariaAttribute "flowto"

ariaHaspopup :: forall msg. Boolean -> Prop msg
ariaHaspopup = ariaAttribute "haspopup" <<< show

ariaHidden :: forall msg. Boolean -> Prop msg
ariaHidden = ariaAttribute "hidden" <<< show

ariaInvalid :: forall msg. Boolean -> Prop msg
ariaInvalid = ariaAttribute "invalid" <<< show

ariaKeyshortcuts :: forall msg. String -> Prop msg
ariaKeyshortcuts = ariaAttribute "keyshortcuts"

ariaLabel :: forall msg. String -> Prop msg
ariaLabel = ariaAttribute "label"

ariaLabelledby :: forall msg. String -> Prop msg
ariaLabelledby = ariaAttribute "labelledby"

ariaLevel :: forall msg. Int -> Prop msg
ariaLevel = ariaAttribute "level" <<< show

ariaLive :: forall msg. String -> Prop msg
ariaLive = ariaAttribute "live"

ariaModal :: forall msg. Boolean -> Prop msg
ariaModal = ariaAttribute "modal" <<< show

ariaMultiline :: forall msg. Boolean -> Prop msg
ariaMultiline = ariaAttribute "multiline" <<< show

ariaMultiselectable :: forall msg. Boolean -> Prop msg
ariaMultiselectable = ariaAttribute "multiselectable" <<< show

ariaOrientation :: forall msg. String -> Prop msg
ariaOrientation = ariaAttribute "orientation"

ariaOwns :: forall msg. String -> Prop msg
ariaOwns = ariaAttribute "owns"

ariaPlaceholder :: forall msg. String -> Prop msg
ariaPlaceholder = ariaAttribute "placeholder"

ariaPosinset :: forall msg. Int -> Prop msg
ariaPosinset = ariaAttribute "posinset" <<< show

ariaPressed :: forall msg. Boolean -> Prop msg
ariaPressed = ariaAttribute "pressed" <<< show

ariaReadonly :: forall msg. Boolean -> Prop msg
ariaReadonly = ariaAttribute "readonly" <<< show

ariaRelevant :: forall msg. String -> Prop msg
ariaRelevant = ariaAttribute "relevant"

ariaRequired :: forall msg. Boolean -> Prop msg
ariaRequired = ariaAttribute "required" <<< show

ariaRoledescription :: forall msg. String -> Prop msg
ariaRoledescription = ariaAttribute "roledescription"

ariaRowcount :: forall msg. Int -> Prop msg
ariaRowcount = ariaAttribute "rowcount" <<< show

ariaRowindex :: forall msg. Int -> Prop msg
ariaRowindex = ariaAttribute "rowindex" <<< show

ariaRowindextext :: forall msg. String -> Prop msg
ariaRowindextext = ariaAttribute "rowindextext"

ariaRowspan :: forall msg. Int -> Prop msg
ariaRowspan = ariaAttribute "rowspan" <<< show

ariaSelected :: forall msg. Boolean -> Prop msg
ariaSelected = ariaAttribute "selected" <<< show

ariaSetsize :: forall msg. Int -> Prop msg
ariaSetsize = ariaAttribute "setsize" <<< show

ariaSort :: forall msg. String -> Prop msg
ariaSort = ariaAttribute "sort"

ariaValuemax :: forall msg. Number -> Prop msg
ariaValuemax = ariaAttribute "valuemax" <<< show

ariaValuemin :: forall msg. Number -> Prop msg
ariaValuemin = ariaAttribute "valuemin" <<< show

ariaValuenow :: forall msg. Number -> Prop msg
ariaValuenow = ariaAttribute "valuenow" <<< show

ariaValuetext :: forall msg. String -> Prop msg
ariaValuetext = ariaAttribute "valuetext"

-- End ARIA attributes

accept :: forall msg. String -> Prop msg
accept = attribute "accept"

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

contenteditable :: forall msg. String -> Prop msg
contenteditable = attribute "contenteditable"

clipRule :: forall msg. String -> Prop msg
clipRule = attribute "clip-rule"

colSpan :: forall msg. Int -> Prop msg
colSpan = attribute "colSpan" <<< show

cRadius :: forall msg. String -> Prop msg
cRadius = attribute "r"

crossorigin :: forall msg. String -> Prop msg
crossorigin = attribute "crossorigin"

cxCoord :: forall msg. String -> Prop msg
cxCoord = attribute "cx"

cyCoord :: forall msg. String -> Prop msg
cyCoord = attribute "cy"

data_ :: forall msg. String -> Prop msg
data_ = attribute "data"

dataCustom :: forall msg. String -> String -> Prop msg
dataCustom key dataValue = attribute ("data-" <> key) dataValue

defaultValue :: forall msg. String -> Prop msg
defaultValue = attribute "defaultValue"

defer :: forall msg. String -> Prop msg
defer = attribute "defer"

dims :: forall msg. String -> Prop msg
dims = attribute "d"

dir :: forall msg. String -> Prop msg
dir = attribute "dir"

dirname :: forall msg. String -> Prop msg
dirname = attribute "dirname"

disabled :: forall msg. String -> Prop msg
disabled = attribute "disabled"

draggable :: forall msg. Boolean -> Prop msg
draggable = attribute "draggable" <<< show

elementtiming :: forall msg. String -> Prop msg
elementtiming = attribute "elementtiming"

enctype :: forall msg. String -> Prop msg
enctype = attribute "enctype"

enterkeyhint :: forall msg. String -> Prop msg
enterkeyhint = attribute "enterkeyhint"

exportparts :: forall msg. String -> Prop msg
exportparts = attribute "exportparts"

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

hidden :: forall msg. String -> Prop msg
hidden = attribute "hidden"

href :: forall msg. String -> Prop msg
href = attribute "href"

id_ :: forall msg. String -> Prop msg
id_ = attribute "id"

inert :: forall msg. Prop msg
inert = attribute "inert" ""

inputmode :: forall msg. String -> Prop msg
inputmode = attribute "inputmode"

itemid :: forall msg. String -> Prop msg
itemid = attribute "itemid"

itemprop :: forall msg. String -> Prop msg
itemprop = attribute "itemprop"

itemref :: forall msg. String -> Prop msg
itemref = attribute "itemref"

itemscope :: forall msg. Prop msg
itemscope = attribute "itemscope" ""

itemtype :: forall msg. String -> Prop msg
itemtype = attribute "itemtype"

lang :: forall msg. String -> Prop msg
lang = attribute "lang"

nonce :: forall msg. String -> Prop msg
nonce = attribute "nonce"

max :: forall msg. String -> Prop msg
max = attribute "max"

maxlength :: forall msg. Int -> Prop msg
maxlength = attribute "maxlength" <<< show

method :: forall msg. String -> Prop msg
method = attribute "method"

min :: forall msg. String -> Prop msg
min = attribute "min"

minlength :: forall msg. Int -> Prop msg
minlength = attribute "minlength" <<< show

multiple :: forall msg. Prop msg
multiple = attribute "multiple" ""

name :: forall msg. String -> Prop msg
name = attribute "name"

-- "no-op" Prop renders no attribute
noop :: forall msg. Prop msg
noop = attribute "" ""

offset :: forall msg. String -> Prop msg
offset = attribute "offset"

part :: forall msg. String -> Prop msg
part = attribute "part"

pattern :: forall msg. String -> Prop msg
pattern = attribute "pattern"

placeholder :: forall msg. String -> Prop msg
placeholder = attribute "placeholder"

points :: forall msg. String -> Prop msg
points = attribute "points"

popover :: forall msg. Prop msg
popover = attribute "popover" ""

popovertarget :: forall msg. String -> Prop msg
popovertarget = attribute "popovertarget"

readonly :: forall msg. Prop msg
readonly = attribute "readonly" ""

required :: forall msg. Prop msg
required = attribute "required" ""

role :: forall msg. String -> Prop msg
role = attribute "role"

rows :: forall msg. Int -> Prop msg
rows = attribute "rows" <<< show

rx :: forall msg. String -> Prop msg
rx = attribute "rx"

selected :: forall msg. String -> Prop msg
selected = attribute "selected"

size :: forall msg. Int -> Prop msg
size = attribute "size" <<< show

slot :: forall msg. String -> Prop msg
slot = attribute "slot"

spellcheck :: forall msg. String -> Prop msg
spellcheck = attribute "spellcheck"

src :: forall msg. String -> Prop msg
src = attribute "src"

step :: forall msg. Int -> Prop msg
step = attribute "step" <<< show

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

translate :: forall msg. String -> Prop msg
translate = attribute "translate"

type_ :: forall msg. String -> Prop msg
type_ = attribute "type"

value :: forall msg. String -> Prop msg
value = attribute "value"

viewBox :: forall msg. String -> Prop msg
viewBox = attribute "viewBox"

width :: forall msg. String -> Prop msg
width = attribute "width"

writingsuggestions :: forall msg. Boolean -> Prop msg
writingsuggestions = attribute "writingsuggestions" <<< show

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
