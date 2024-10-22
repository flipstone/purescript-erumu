module DOM.Erumu.Widget.CheckboxInput
  ( Model
  , Msg
  , empty
  , fill
  , isChecked
  , render
  , renderWith
  , update
  ) where

import Prelude

import DOM.Erumu.HTML (input, type_, checked)
import DOM.Erumu.HTML.Decoder (inputChecked)
import DOM.Erumu.Types (HTML, UpdateResult, Prop, onEventDecode, (!))

newtype Model = Model Boolean
newtype Msg = Msg Boolean

empty :: Model
empty = Model false

fill :: Boolean -> Model -> Model
fill b _ = Model b

isChecked :: Model -> Boolean
isChecked (Model b) = b

render :: Model -> HTML Msg
render = renderWith identity []

renderWith ::
  forall msg.
  (Msg -> msg) ->
  Array (Prop msg) ->
  Model ->
  HTML msg
renderWith liftMsg userProps (Model m) =
  let
    checkedProp =
      if m then [ checked "checked" ]
      else []
    ourProps =
      [ type_ "checkbox"
      , onEventDecode "onclick" (liftMsg <<< Msg <$> inputChecked)
      ]

  in
    input (checkedProp <> ourProps <> userProps) []

update ::
  forall m.
  Applicative m =>
  Msg ->
  Model ->
  UpdateResult m Model Msg
update (Msg checked) _ = Model checked ! []
