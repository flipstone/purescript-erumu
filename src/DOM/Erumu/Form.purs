module DOM.Erumu.Form
  ( Model
  , Msg
  , empty
  , textField
  , passwordField
  , selectField
  , textAreaField
  , update
  , readModel
  , getField
  , setField
  , nest
  , disabledProp
  , placeholderProp
  ) where

import Prelude

import Foreign (Foreign, F, readString)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as Foreign

import DOM.Erumu.Decode (Decode)
import DOM.Erumu.HTML (type_, input, select, value, textArea, disabled, placeholder)
import DOM.Erumu.HTML.Decoder (textAreaValue, selectValue, inputValue)
import DOM.Erumu.Types (HTML, Prop, onEventDecode)

data Model
  = Field String
  | Object (Foreign.Object Model)

readModel :: Foreign -> F Model
readModel f = Field <$> readString f

--case readString f of
--  Right _ -> pure $ Field "foo"
--  Left _ -> pure $ Field "bar"
--  errorKeys <- keys f

--  let readKey key = do child <- prop key f
--                       formErrors <- readModel child
--                       pure (Tuple key formErrors)

--  Object <$> Foreign.fromFoldable <$> traverse readKey errorKeys

setField :: List String -> String -> Model -> Model
setField Nil value _ = Field value
setField (Cons key rest) value (Field _) =
  Object (Foreign.singleton key (setField rest value (Field "")))

setField (Cons key rest) value (Object map) =
  Object (Foreign.alter alter key map)
  where
  alter Nothing = Just (setField rest value (Field ""))
  alter (Just model) = Just (setField rest value model)

getChild :: String -> Model -> Model
getChild key (Object map) =
  fromMaybe empty (Foreign.lookup key map)

getChild _ _ = empty

getField :: List String -> Model -> String
getField Nil (Field s) = s
getField (Cons key rest) (Object map) =
  case Foreign.lookup key map of
    Just subForm -> getField rest subForm
    Nothing -> ""

getField _ _ = ""

data Msg =
  SetField (List String) String

empty :: Model
empty = Object Foreign.empty

nest ::
  forall msg.
  String ->
  Model ->
  ((Msg -> Msg) -> msg -> msg) ->
  (Model -> HTML msg) ->
  HTML msg
nest key model lift render =
  lift pushKey <$> render subModel
  where
  subModel = getChild key model
  pushKey (SetField path value) = SetField (Cons key path) value

disabledProp :: forall msg. Boolean -> Array (Prop msg)
disabledProp = if _ then [ disabled "disabled" ] else []

placeholderProp :: forall msg. Maybe String -> Array (Prop msg)
placeholderProp Nothing = []
placeholderProp (Just s) = [ placeholder s ]

textField :: Array (Prop Msg) -> Model -> String -> HTML Msg
textField userProps model key =
  let
    ourProps =
      [ type_ "text"
      , onEventDecode "oninput" (decodeInputSetField (Cons key Nil))
      , value (getField (Cons key Nil) model)
      ]
  in
    input (ourProps <> userProps) []

passwordField :: Array (Prop Msg) -> Model -> String -> HTML Msg
passwordField userProps model key =
  let
    ourProps =
      [ type_ "password"
      , onEventDecode "oninput" (decodeInputSetField (Cons key Nil))
      , value (getField (Cons key Nil) model)
      ]
  in
    input (ourProps <> userProps) []

textAreaField :: Array (Prop Msg) -> Model -> String -> HTML Msg
textAreaField userProps model key =
  let
    ourProps =
      [ onEventDecode "oninput" (decodeTextAreaSetField (Cons key Nil))
      , value (getField (Cons key Nil) model)
      ]
  in
    textArea (ourProps <> userProps) []

selectField :: Array (Prop Msg) -> String -> Array (HTML Msg) -> HTML Msg
selectField userProps key children =
  let
    ourProps =
      [ onEventDecode "onchange" (decodeSelectSetField (Cons key Nil))
      ]

  in
    select (ourProps <> userProps) children

update :: Msg -> Model -> Model
update (SetField key value) m = setField key value m

decodeSetField :: Decode String -> List String -> Decode Msg
decodeSetField decoder key = SetField key <$> decoder

--
-- These shims are required to provide type signatures to prevent type variables
-- from escaping their scope at points where `decodeSetField` is used.
--

decodeInputSetField :: List String -> Decode Msg
decodeInputSetField = decodeSetField inputValue

decodeSelectSetField :: List String -> Decode Msg
decodeSelectSetField = decodeSetField selectValue

decodeTextAreaSetField :: List String -> Decode Msg
decodeTextAreaSetField = decodeSetField textAreaValue
