module DOM.Erumu.Widget.TextArea
  ( Msg
  , Model
  , empty
  , withValue
  , setValue
  , value
  , render
  , renderWith
  , update
  ) where

import Prelude

import DOM.Erumu.HTML (textArea)
import DOM.Erumu.HTML as HTML
import DOM.Erumu.HTML.Decoder (textAreaValue)
import DOM.Erumu.Types (UpdateResult, HTML, Prop, onEventDecode, (!))

newtype Model = Model String
newtype Msg = NewInput String

empty :: Model
empty = withValue ""

withValue :: String -> Model
withValue = Model

value :: Model -> String
value (Model s) = s

setValue :: String -> Model -> Model
setValue s = const $ withValue s

render :: Array (Prop Msg) -> Model -> HTML Msg
render = renderWith identity

renderWith :: forall msg.
              (Msg -> msg)
           -> Array (Prop msg)
           -> Model
           -> HTML msg
renderWith liftMsg userProps (Model currentValue) =
  let ourProps = [ onEventDecode "oninput" (liftMsg <<< NewInput <$> textAreaValue)
                 , HTML.defaultValue currentValue
                 ]
  in textArea (ourProps <> userProps) []

update :: forall m. Applicative m
       => Msg -> Model -> UpdateResult m Model Msg
update (NewInput newValue) _ = Model newValue ! []
