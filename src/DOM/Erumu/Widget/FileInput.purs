-- | A file-picker input widget (`type="file"`). Built on
-- | `DOM.Erumu.Widget.Input.Builder`, its value is the `FileList` the browser
-- | reports for the chosen file(s), or `Nothing` when nothing is selected.
module DOM.Erumu.Widget.FileInput
  ( Model(..)
  , Msg
  , disabled
  , empty
  , render
  , renderWith
  , setDisabled
  , setValue
  , update
  , value
  , withValue
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import DOM.Erumu.Types (UpdateResult, HTML, Prop)
import DOM.Erumu.Widget.Input.Builder as InputBuilder
import Web.File.FileList (FileList)

type Model = InputBuilder.Model

type Msg = InputBuilder.Msg

-- | A file input with nothing selected.
empty :: Model
empty = InputBuilder.withValue (InputBuilder.FileInput Nothing) InputBuilder.init

-- | A file input pre-populated with the given selection.
withValue :: Maybe FileList -> Model
withValue maybeFiles = InputBuilder.withValue (InputBuilder.FileInput maybeFiles) InputBuilder.init

-- | The currently selected files, or `Nothing` when nothing is selected (or the
-- | model is not a file input).
value :: Model -> Maybe FileList
value model =
  case InputBuilder.inputTypeValue model of
    InputBuilder.FileInput maybeFiles -> maybeFiles
    _ -> Nothing

-- | Replace the selection. A no-op for a model that is not a file input. Preserves the input's disabled state.
setValue :: Maybe FileList -> Model -> Model
setValue maybeFiles model =
  case InputBuilder.inputTypeValue model of
    InputBuilder.FileInput _ -> InputBuilder.withValue (InputBuilder.FileInput maybeFiles) model
    _ -> model

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
