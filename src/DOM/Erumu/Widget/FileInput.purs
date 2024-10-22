module DOM.Erumu.Widget.FileInput
  ( Msg
  , Model(..)
  , empty
  , withValue
  , value
  , setValue
  , render
  , renderWith
  , update
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import DOM.Erumu.HTML (input, type_)
import DOM.Erumu.HTML.Decoder (inputFiles)
import DOM.Erumu.Types (UpdateResult, HTML, Prop, onEventDecode, (!))
import Web.File.FileList (FileList)

newtype Model = Model (Maybe FileList)
newtype Msg = NewInput (Maybe FileList)

empty :: Model
empty = withValue Nothing

withValue :: Maybe FileList -> Model
withValue = Model

value :: Model -> Maybe FileList
value (Model fl') = fl'

setValue :: Maybe FileList -> Model -> Model
setValue fl' = const $ withValue fl'

render :: Array (Prop Msg) -> Model -> HTML Msg
render = renderWith identity

renderWith ::
  forall msg.
  (Msg -> msg) ->
  Array (Prop msg) ->
  Model ->
  HTML msg
renderWith liftMsg userProps _ =
  let
    ourProps =
      [ type_ "file"
      , onEventDecode "onchange" (liftMsg <<< NewInput <$> inputFiles)
      ]
  in
    input (ourProps <> userProps) []

update ::
  forall m.
  Applicative m =>
  Msg ->
  Model ->
  UpdateResult m Model Msg
update (NewInput newValue) _ = Model newValue ! []
