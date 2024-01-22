module DOM.Erumu.Widget.Select
  ( Msg
  , Model
  , empty
  , selected
  , fill
  , fillAddIfMissing
  , fillBy
  , setDisabled
  , render
  , update
  , itemList
  ) where

import Prelude

import Data.Array (findIndex, elem, snoc, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)

import DOM.Erumu.Form (disabledProp)
import DOM.Erumu.HTML (select, option)
import DOM.Erumu.HTML as HTML
import DOM.Erumu.HTML.Decoder (selectedIndex)
import DOM.Erumu.Types (HTML, Prop, Return(..), text, (!), onEventDecode)

newtype Model a = Model
  { items :: Array a
  , selectedIndex :: Int
  , disabled :: Boolean
  , showFn :: a -> Maybe String
  }

data Msg = Selected Int

empty :: forall a. (a -> Maybe String) -> Array a -> Model a
empty showFn items =
  Model
    { items: items
    , selectedIndex: 0
    , disabled: false
    , showFn: showFn
    }

fill :: forall a. Eq a => a -> Model a -> Model a
fill a = fillBy ((==) a)

fillAddIfMissing :: forall a. Eq a => a -> Model a -> Model a
fillAddIfMissing a originalModel@(Model m) =
  let
    modelWithItem =
      if elem a m.items
      then originalModel
      else Model m { items = snoc m.items a }
  in
    fill a modelWithItem

fillBy :: forall a. (a -> Boolean) -> Model a -> Model a
fillBy pred (Model m) =
  case findIndex pred m.items of
    Nothing -> Model m
    Just idx -> Model m { selectedIndex = idx }

-- It's unfortunate to end up with a Maybe here when
-- the only case we expected that is with an empty
-- array. The only alternative though would be to make
-- this widget not ever support rendering with an empty
-- list, which is probably not what's expected of a
-- general Select widget.
selected :: forall a. Model a -> Maybe a
selected (Model m) = m.items !! m.selectedIndex

setDisabled :: forall a. Boolean -> Model a -> Model a
setDisabled disabled (Model m) =
  Model m { disabled = disabled }

render :: forall a. Array (Prop Msg) -> Model a -> HTML Msg
render userProps model@(Model m) =
  let items = mapWithIndex (renderItem model) m.items
   in select (staticProps <> disabledProp m.disabled <> userProps) items

staticProps :: Array (Prop Msg)
staticProps = [onEventDecode "onchange" (Selected <$> selectedIndex)]

renderItem :: forall a. Model a -> Int -> a -> HTML Msg
renderItem (Model m) idx item =
  let attrs = if m.selectedIndex == idx
              then [HTML.selected "selected"]
              else []

   in option attrs [text $ fromMaybe "Select..." $ m.showFn item]

update :: forall a m. Applicative m => Msg -> Model a -> Return m (Model a) Msg a
update (Selected idx) (Model m) =
  let newModel = Model m { selectedIndex = idx }
   in case selected newModel of
        Just v -> Return v $ newModel ! []
        Nothing -> Continue $ newModel ! []

itemList :: forall a. Model a -> Array a
itemList (Model m) = m.items
