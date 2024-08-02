module DOM.Erumu.Widget.RadioInputGroup
  ( Model
  , Msg
  , empty
  , selected
  , fill, fillBy
  , itemEventProp
  , render
  , renderOne
  , update
  ) where

import Prelude

import Data.Array ((!!), findIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import DOM.Erumu.HTML (checked, class_, div_, input, label, name, type_)
import DOM.Erumu.HTML.Decoder (inputChecked)
import DOM.Erumu.Types (HTML, Prop, onEventDecode, noElement, text)

newtype Model a = Model
  { groupName     :: String
  , label         :: a -> String
  , items         :: Array a
  , selectedIndex :: Int
  }

data Msg = Selected Int Boolean

empty :: forall a. String -> (a -> String) -> Array a -> Model a
empty groupName labelFn items = Model
  { groupName: groupName
  , label: labelFn
  , items: items
  , selectedIndex: 0
  }

fill :: forall a. Eq a => a -> Model a -> Model a
fill a = fillBy ((==) a)

fillBy :: forall a. (a -> Boolean) -> Model a -> Model a
fillBy pred (Model m) =
  case findIndex pred m.items of
    Nothing -> Model m
    Just idx -> Model m { selectedIndex = idx }

render :: forall a. Model a -> HTML Msg
render = renderWith []

renderWith :: forall a. Array (Prop Msg) -> Model a -> HTML Msg
renderWith userProps model@(Model m) =
  div_ userProps
    (mapWithIndex (renderItem model) m.items)

renderOne :: forall a. Model a -> Int -> HTML Msg
renderOne = renderOneWith []

renderOneWith :: forall a. Array (Prop Msg) -> Model a -> Int -> HTML Msg
renderOneWith userProps model@(Model m) idx =
  case m.items !! idx of
    Nothing -> noElement
    Just item -> div_ userProps
                   [ renderItem model idx item ]

renderItem :: forall a. Model a -> Int -> a -> HTML Msg
renderItem (Model m) idx item =
  let checkedProp = if m.selectedIndex == idx
                      then [ checked "checked" ]
                      else []
      ourProps = [ type_ "radio"
                 , itemEventProp idx
                 , name m.groupName
                 ]

   in div_ []
        [ input (checkedProp <> ourProps) []
        , label [ class_ "margin-5-left margin-3-above"]
            [ text $ m.label item
            ]
        ]

itemEventProp :: Int -> Prop Msg
itemEventProp idx = onEventDecode "onclick" (Selected idx <$> inputChecked)

update :: forall a. Msg -> Model a -> Model a
update (Selected idx _) (Model m) =
  Model m { selectedIndex = idx }

selected :: forall a. Model a -> Maybe a
selected (Model m) = m.items !! m.selectedIndex

