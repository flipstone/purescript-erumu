module DOM.Erumu.HTML.Events
  ( onclick
  , preventDefaultOnclick
  , terminalOnclick
  , clickawayfn
  , oninput
  , onblur
  , onfocus
  , onmouseenter
  , onmouseleave
  ) where


import DOM.Erumu.Types (onEvent, onPreventDefaultEvent, onPropagatingEvent, Prop)

onclick :: forall msg. msg -> Prop msg
onclick = onPropagatingEvent "onclick"

terminalOnclick :: forall msg. msg -> Prop msg
terminalOnclick = onEvent "onclick"

preventDefaultOnclick :: forall msg. msg -> Prop msg
preventDefaultOnclick = onPreventDefaultEvent "onclick"

oninput :: forall msg. msg -> Prop msg
oninput = onEvent "oninput"

onblur :: forall msg. msg -> Prop msg
onblur = onEvent "onblur"

onfocus :: forall msg. msg -> Prop msg
onfocus = onEvent "onfocus"

onmouseenter :: forall msg. msg -> Prop msg
onmouseenter = onEvent "onmouseenter"

onmouseleave :: forall msg. msg -> Prop msg
onmouseleave = onEvent "onmouseleave"

clickawayfn :: forall msg. msg -> Prop msg
clickawayfn = onEvent "clickawayfn"
