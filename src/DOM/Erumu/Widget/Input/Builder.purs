-- | The shared implementation backing the concrete input widgets
-- | (`DOM.Erumu.Widget.TextInput`, `DOM.Erumu.Widget.FileInput`,
-- | `DOM.Erumu.Widget.CheckboxInput`, and friends).
-- |
-- | A single `Model` holds the state for any flavour of `<input>` element. The
-- | flavour is selected by the `InputTypeValue` it carries, which determines
-- | both the rendered `type` attribute and how user interaction is decoded back
-- | into a `Msg`. The concrete widget modules are thin wrappers that fix a
-- | particular `InputTypeValue` and expose a type-specific API on top of this
-- | builder.
module DOM.Erumu.Widget.Input.Builder
  ( InputTypeValue(..)
  , Model
  , Msg
  , disabled
  , init
  , inputTypeValue
  , renderWith
  , setDisabled
  , update
  , withValue
  ) where

import Prelude

import Data.Maybe (Maybe)
import DOM.Erumu.HTML.Attributes (noop)
import DOM.Erumu.HTML.Decoder (inputChecked, inputFiles, inputValue)
import DOM.Erumu.Form (disabledProp)
import DOM.Erumu.HTML (checked, input, type_, value)
import DOM.Erumu.Types (HTML, UpdateResult, Prop, onEventDecode, (!))
import Web.File.FileList (FileList)

-- | The opaque state of an input widget. Construct one with `init` and refine
-- | it with `withValue`/`setDisabled`; inspect it with `inputTypeValue`,
-- | `disabled`, and the accessors on the concrete widget modules.
newtype Model = Model Fields

type Fields =
  { disabled :: Boolean
  , inputTypeValue :: InputTypeValue
  }

-- | The kind of `<input>` to render together with its current value. Each
-- | constructor maps to an HTML `type` attribute and pairs the input with the
-- | value type appropriate for it (a `Boolean` for checkboxes, a `FileList` for
-- | file inputs, a `String` for the text-like inputs).
data InputTypeValue
  = CheckboxInput Boolean
  | EmailInput String
  | FileInput (Maybe FileList)
  | TextInput String
  | PasswordInput String

-- | The builder currently does not support these flavours of `<input>`:
-- | Button
-- | Color
-- | Date
-- | DatetimeLocal
-- | Hidden
-- | Image
-- | Month
-- | Number
-- | Radio
-- | Range
-- | Reset
-- | Search
-- | Submit
-- | Tel
-- | Time
-- | Url
-- | Week

-- | An event emitted by a rendered input carrying the new value the user
-- | produced. There is one constructor per `InputTypeValue` flavour; feeding it
-- | to `update` folds the new value back into the `Model`.
data Msg
  = CheckboxMsg Boolean
  | EmailMsg String
  | FileMsg (Maybe FileList)
  | TextMsg String
  | PasswordMsg String

-- | An enabled, empty text input. This is the starting point every widget's
-- | `empty`/`withValue` constructor builds on; override the value with
-- | `withValue` to choose a different input flavour.
init :: Model
init = Model
  { disabled: false
  , inputTypeValue: TextInput ""
  }

-- | Replace the model's input type and value. This is how a `Model` switches
-- | between (or refreshes the value of) checkbox, email, file, text, and
-- | password inputs. The disabled state is preserved.
withValue :: InputTypeValue -> Model -> Model
withValue (CheckboxInput checkedValue) (Model m) =
  Model $ m { inputTypeValue = CheckboxInput checkedValue }

withValue (EmailInput emailValue) (Model m) =
  Model $ m { inputTypeValue = EmailInput emailValue }

withValue (FileInput maybeFiles) (Model m) =
  Model $ m { inputTypeValue = FileInput maybeFiles }

withValue (TextInput textValue) (Model m) =
  Model $ m { inputTypeValue = TextInput textValue }

withValue (PasswordInput passwordValue) (Model m) =
  Model $ m { inputTypeValue = PasswordInput passwordValue }

setDisabled :: Boolean -> Model -> Model
setDisabled bool (Model m) =
  Model $ m { disabled = bool }

disabled :: Model -> Boolean
disabled (Model m) = m.disabled

-- | The model's current input type tagged with its value. Concrete widgets
-- | pattern match on this to project out the value in their own type.
inputTypeValue :: Model -> InputTypeValue
inputTypeValue (Model m) = m.inputTypeValue

renderWith :: forall msg. (Msg -> msg) -> Array (Prop msg) -> Model -> HTML msg
renderWith liftMsg userProps (Model m) =
  let
    ourProps =
      case m.inputTypeValue of
        CheckboxInput checkValue ->
          [ type_ "checkbox"
          , onEventDecode "onclick" (liftMsg <<< CheckboxMsg <$> inputChecked)
          , if checkValue then (checked "checked") else noop
          ]
        EmailInput emailValue ->
          [ type_ "email"
          , onEventDecode "oninput" (liftMsg <<< EmailMsg <$> inputValue)
          , value emailValue
          ]
        FileInput _fileValue ->
          [ type_ "file"
          , onEventDecode "onchange" (liftMsg <<< FileMsg <$> inputFiles)
          ]
        TextInput textValue ->
          [ type_ "text"
          , onEventDecode "oninput" (liftMsg <<< TextMsg <$> inputValue)
          , value textValue
          ]
        PasswordInput passwordValue ->
          [ type_ "password"
          , onEventDecode "oninput" (liftMsg <<< PasswordMsg <$> inputValue)
          , value passwordValue
          ]

  in
    input (ourProps <> disabledProp m.disabled <> userProps) []

update :: forall m. Applicative m => Msg -> Model -> UpdateResult m Model Msg
update (CheckboxMsg checked) (Model m) =
  Model (m { inputTypeValue = CheckboxInput checked }) ! []
update (EmailMsg email) (Model m) =
  Model (m { inputTypeValue = EmailInput email }) ! []
update (FileMsg maybeFiles) (Model m) =
  Model (m { inputTypeValue = FileInput maybeFiles }) ! []
update (TextMsg text) (Model m) =
  Model (m { inputTypeValue = TextInput text }) ! []
update (PasswordMsg password) (Model m) =
  Model (m { inputTypeValue = PasswordInput password }) ! []
