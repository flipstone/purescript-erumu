module DOM.Erumu.Array.Dynamic
  ( Msg
  , Edit, add, remove, editMsg, updateMsg
  , render
  , update
  ) where

import Prelude

import Data.Array (snoc, deleteAt)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import DOM.Erumu.Array as Array
import DOM.Erumu.Types (UpdateResult, HTML, liftUpdate, (!))

data Edit model =
    Add model
  | Remove Int

add :: forall model. model -> Edit model
add = Add

remove :: forall model. Int -> Edit model
remove = Remove

editMsg :: forall model msg. Edit model -> Msg model msg
editMsg = Msg <<< Left

updateMsg :: forall model msg. (Array.Msg msg) -> Msg model msg
updateMsg = Msg <<< Right

newtype Msg model msg =
  Msg (Either (Edit model) (Array.Msg msg))

render :: forall model msg.
          (Int -> model -> HTML (Either (Edit model) msg))
       -> Array model
       -> Array (HTML (Msg model msg))
render renderItem items =
  let renderItem' lifter idx model = Msg <<< map lifter <$> renderItem idx model
   in Array.renderLift renderItem' items

update :: forall m msg model.
          Applicative m
       => (msg -> model -> UpdateResult m model msg)
       -> Msg model msg
       -> Array model
       -> Maybe (UpdateResult m (Array model) (Msg model msg))
update _ (Msg (Left (Add model))) items = Just $
  (items `snoc` model) ! []

update _ (Msg (Left (Remove idx))) oldItems = Just $
  case deleteAt idx oldItems of
    Nothing -> oldItems ! []
    Just newItems -> newItems ! []

update updateItem (Msg (Right msg)) items =
  liftUpdate (Msg <<< Right) identity <$>
    Array.update updateItem msg items

