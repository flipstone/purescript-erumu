-- | A checkbox input widget (`type="checkbox"`). Built on
-- | `DOM.Erumu.Widget.Input.Builder`, its value is the `Boolean` checked state.
module DOM.Erumu.Widget.CheckboxInput
  ( Model
  , Msg
  , disabled
  , empty
  , fill
  , isChecked
  , render
  , renderWith
  , setDisabled
  , update
  ) where

import Prelude

import DOM.Erumu.Types (HTML, UpdateResult, Prop)
import DOM.Erumu.Widget.Input.Builder as InputBuilder

type Model = InputBuilder.Model

type Msg = InputBuilder.Msg

-- | An unchecked checkbox.
empty :: Model
empty = InputBuilder.withValue (InputBuilder.CheckboxInput false) InputBuilder.init

-- | Set the checked state while preserving the input's disabled state.
-- | A no-op for models that are not checkboxes.
fill :: Boolean -> Model -> Model
fill b model = InputBuilder.withValue (InputBuilder.CheckboxInput b) model

-- | Whether the checkbox is checked. Returns `false` for a model that is not a
-- | checkbox.
isChecked :: Model -> Boolean
isChecked model =
  case InputBuilder.inputTypeValue model of
    InputBuilder.CheckboxInput b -> b
    _ -> false

setDisabled :: Boolean -> Model -> Model
setDisabled bool model =
  InputBuilder.setDisabled bool model

disabled :: Model -> Boolean
disabled model = InputBuilder.disabled model

render :: Model -> HTML Msg
render = renderWith identity []

renderWith ::
  forall msg.
  (Msg -> msg) ->
  Array (Prop msg) ->
  Model ->
  HTML msg
renderWith liftMsg userProps model =
  InputBuilder.renderWith liftMsg userProps model

update ::
  forall m.
  Applicative m =>
  Msg ->
  Model ->
  UpdateResult m Model Msg
update msg model =
  InputBuilder.update msg model
