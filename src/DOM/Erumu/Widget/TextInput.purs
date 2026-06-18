-- | A single-line text input widget. Built on `DOM.Erumu.Widget.Input.Builder`,
-- | it covers the three text-like input flavours that carry a `String` value:
-- | plain text (`withValue`), email (`withEmailValue`), and password
-- | (`withPasswordValue`). The flavour chosen at construction is preserved by
-- | the value accessors and `setValue`.
module DOM.Erumu.Widget.TextInput
  ( Model
  , Msg
  , disabled
  , empty
  , fill
  , isBlank
  , render
  , renderWith
  , setDisabled
  , setValue
  , update
  , value
  , withEmailValue
  , withPasswordValue
  , withValue
  ) where

import Prelude

import DOM.Erumu.Types (HTML, Prop, UpdateResult)
import DOM.Erumu.Widget.Input.Builder as InputBuilder

type Model = InputBuilder.Model

type Msg = InputBuilder.Msg

-- | An empty plain-text input.
empty :: Model
empty = InputBuilder.init

-- | A plain-text input pre-filled with the given value.
withValue :: String -> Model
withValue s = InputBuilder.withValue (InputBuilder.TextInput s) InputBuilder.init

-- | An email input (`type="email"`) pre-filled with the given value.
withEmailValue :: String -> Model
withEmailValue s = InputBuilder.withValue (InputBuilder.EmailInput s) InputBuilder.init

-- | A password input (`type="password"`) pre-filled with the given value.
withPasswordValue :: String -> Model
withPasswordValue s = InputBuilder.withValue (InputBuilder.PasswordInput s) InputBuilder.init

-- | The current string value. Returns `""` for a model that is not one of the
-- | text-like flavours (e.g. a checkbox or file input).
value :: Model -> String
value model =
  case InputBuilder.inputTypeValue model of
    InputBuilder.CheckboxInput _ -> ""
    InputBuilder.EmailInput emailValue -> emailValue
    InputBuilder.FileInput _ -> ""
    InputBuilder.TextInput textValue -> textValue
    InputBuilder.PasswordInput passwordValue -> passwordValue

-- | Whether the input's value is empty.
isBlank :: Model -> Boolean
isBlank m = eq "" (value m)

-- | Replace the value while preserving the input flavour and disabled state. A no-op for models
-- | that are not text-like.
setValue :: String -> Model -> Model
setValue s model =
  case InputBuilder.inputTypeValue model of
    InputBuilder.CheckboxInput _ -> model
    InputBuilder.EmailInput _ -> InputBuilder.withValue (InputBuilder.EmailInput s) model
    InputBuilder.FileInput _ -> model
    InputBuilder.TextInput _ -> InputBuilder.withValue (InputBuilder.TextInput s) model
    InputBuilder.PasswordInput _ -> InputBuilder.withValue (InputBuilder.PasswordInput s) model

-- | Alias for `setValue`: replace the input's value.
fill :: String -> Model -> Model
fill s model = setValue s model

setDisabled :: Boolean -> Model -> Model
setDisabled bool model =
  InputBuilder.setDisabled bool model

disabled :: Model -> Boolean
disabled model = InputBuilder.disabled model

render :: Array (Prop Msg) -> Model -> HTML Msg
render = renderWith identity

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
