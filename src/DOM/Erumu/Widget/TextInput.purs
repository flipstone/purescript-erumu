module DOM.Erumu.Widget.TextInput
  ( Model
  , Msg
  , disabled
  , empty
  , isBlank
  , render
  , renderWith
  , setDisabled
  , setEmailInputType
  , setPasswordInputType
  , setValue
  , update
  , value
  , withValue
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import DOM.Erumu.Form (disabledProp)
import DOM.Erumu.HTML (input, type_)
import DOM.Erumu.HTML as HTML
import DOM.Erumu.HTML.Decoder (inputValue)
import DOM.Erumu.Types (UpdateResult, HTML, Prop, onEventDecode, (!))

newtype Model = Model Fields

type Fields =
  { currentValue :: String
  , disabled :: Boolean
  , inputType :: InputType
  }

newtype Msg = NewInput String

newtype InputType = InputType String

derive instance Newtype InputType _

-- | The value of the input together with its type, which determines how
-- | the value is rendered and updated. The builder defaults to text,
-- | but also supports password and email inputs using the `setPasswordInputType` and `setEmailInputType` functions.
empty :: Model
empty = withValue ""

-- | Create a new `Model` with the given value, preserving the default input type (text) and disabled state (false).
withValue :: String -> Model
withValue s =
  Model
    { currentValue: s
    , disabled: false
    , inputType: InputType "text"
    }

value :: Model -> String
value (Model m) = m.currentValue

isBlank :: Model -> Boolean
isBlank (Model m) = eq "" m.currentValue

setValue :: String -> Model -> Model
setValue s (Model m) =
  Model m { currentValue = s }

setDisabled :: Boolean -> Model -> Model
setDisabled bool (Model m) =
  Model m { disabled = bool }

-- | Set the input type to "password", which renders the value as obscured dots and prevents autofill.
setPasswordInputType :: Model -> Model
setPasswordInputType (Model m) =
  Model m { inputType = InputType "password" }

-- | Set the input type to "email", which provides email-specific input behavior and validation.
setEmailInputType :: Model -> Model
setEmailInputType (Model m) =
  Model m { inputType = InputType "email" }

disabled :: Model -> Boolean
disabled (Model m) = m.disabled

render :: Array (Prop Msg) -> Model -> HTML Msg
render = renderWith identity

renderWith ::
  forall msg.
  (Msg -> msg) ->
  Array (Prop msg) ->
  Model ->
  HTML msg
renderWith liftMsg userProps (Model m) =
  let
    ourProps =
      [ type_ (unwrap m.inputType)
      , onEventDecode "oninput" (liftMsg <<< NewInput <$> inputValue)
      , HTML.value m.currentValue
      ]
  in
    input (ourProps <> disabledProp m.disabled <> userProps) []

update ::
  forall m.
  Applicative m =>
  Msg ->
  Model ->
  UpdateResult m Model Msg
update (NewInput newValue) model = setValue newValue model ! []
