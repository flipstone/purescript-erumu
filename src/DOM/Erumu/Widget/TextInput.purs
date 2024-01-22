module DOM.Erumu.Widget.TextInput
  ( Msg
  , Model
  , empty
  , withValue
  , setValue
  , setDisabled
  , disabled
  , value
  , isBlank
  , render
  , renderWith
  , update
  ) where

import Prelude

import DOM.Erumu.Form (disabledProp)
import DOM.Erumu.HTML (input, type_)
import DOM.Erumu.HTML as HTML
import DOM.Erumu.HTML.Decoder (inputValue)
import DOM.Erumu.Types (UpdateResult, HTML, Prop, onEventDecode, (!))

newtype Model = Model Fields

type Fields =
  { currentValue :: String
  , disabled     :: Boolean
  }

newtype Msg = NewInput String

empty :: Model
empty = withValue ""

withValue :: String -> Model
withValue s =
  Model { currentValue: s
        , disabled:     false
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

disabled :: Model -> Boolean
disabled (Model m) = m.disabled

render :: Array (Prop Msg) -> Model -> HTML Msg
render = renderWith identity

renderWith :: forall msg.
              (Msg -> msg)
           -> Array (Prop msg)
           -> Model
           -> HTML msg
renderWith liftMsg userProps (Model m) =
  let ourProps =
        [ type_ "text"
        , onEventDecode "oninput" (liftMsg <<< NewInput <$> inputValue)
        , HTML.value m.currentValue
        ]
  in input (ourProps <> disabledProp m.disabled <> userProps) []

update :: forall m. Applicative m
       => Msg -> Model -> UpdateResult m Model Msg
update (NewInput newValue) model = setValue newValue model ! []
