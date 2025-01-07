{-

DOM.Erumu.Array provides some useful utilities for handling message generation
and dispatch of arrays of items. This is appropriate for cases where

A) the list of items is sufficiently small (because updating an individual
   item requires copying the whole array)

B) the container does not produce any messages *simultaneously* (generating
   2 messages from a *single* event or update) where some are routed to
   items in the array and some alter the indexing of items in the array.

Condition B is necessary because the index of the item in the array is captured
in the array Msg type and used for routing during processing.

-}
module DOM.Erumu.Array
  ( Msg(..)
  , render
  , renderConcat
  , renderLift
  , update
  , updateArrayWith
  , updateF
  ) where

import Prelude

import DOM.Erumu.Types ((!), HTML, Return(..), UpdateResult, liftReturn, liftUpdate)
import Data.Array (mapWithIndex, updateAt, (!!))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)

data Msg msg = Msg Int msg

render ::
  forall msg model.
  (model -> HTML msg) ->
  Array model ->
  Array (HTML (Msg msg))
render renderItem items =
  mapWithIndex renderIdx items
  where
  renderIdx idx item = (Msg idx) <$> renderItem item

-- renderLift allows you to render items that may contain a mixture of
-- messages from parent a child (item) components. This can be useful in
-- cases where a parent component has item specific messages that are
-- rendered in the same rendering function as the child component. The lifting
-- function provided allows the rendering function to lift the child messages
-- into Array messages which will then need to be wrapped into parentMsgs using
-- the appropriate parent-specific constructor
-- The index of the item being rendered is provided for your convenience.
renderLift ::
  forall childMsg parentMsg model.
  ((childMsg -> Msg childMsg) -> Int -> model -> HTML parentMsg) ->
  Array model ->
  Array (HTML parentMsg)
renderLift renderItem items =
  mapWithIndex renderIdx items
  where
  renderIdx idx item = renderItem (Msg idx) idx item

renderConcat ::
  forall msg model.
  (model -> Array (HTML msg)) ->
  Array model ->
  Array (HTML (Msg msg))
renderConcat renderItem items =
  foldMapWithIndex renderIdx items
  where
  renderIdx idx item = map (Msg idx) <$> (renderItem item)

update ::
  forall m msg model.
  Applicative m =>
  (msg -> model -> UpdateResult m model msg) ->
  Msg msg ->
  Array model ->
  Maybe (UpdateResult m (Array model) (Msg msg))
update f msg items =
  unwrap $ sequence $ updateF (Identity `compose2` f) msg items
  where
  compose2 ::
    forall a b c d.
    (c -> d) ->
    (a -> b -> c) ->
    (a -> b -> d)
  compose2 g h = \a b -> g (h a b)

updateF ::
  forall f m msg model.
  Functor f =>
  Applicative m =>
  (msg -> model -> f (UpdateResult m model msg)) ->
  Msg msg ->
  Array model ->
  Maybe (f (UpdateResult m (Array model) (Msg msg)))
updateF updateItem (Msg idx msg) items =
  case items !! idx of
    Nothing -> Nothing
    Just oldItem -> Just $
      let
        liftItem newItem = fromMaybe items $ updateAt idx newItem items
      in
        liftUpdate (Msg idx) (liftItem) <$>
          updateItem msg oldItem

-- Return
updateArrayWith ::
  forall m signal childmodel childmsg.
  Applicative m =>
  (childmsg -> childmodel -> Return m childmodel childmsg signal) ->
  Msg childmsg ->
  Array childmodel ->
  Return m (Array childmodel) (Msg childmsg) signal
updateArrayWith updateFn (Msg idx msg) models =
  case models !! idx of

    -- When index isn't in list (how?!), just Continue
    Nothing -> Continue $ models ! []

    Just oldItem ->
      let
        liftModel newM = fromMaybe models $ updateAt idx newM models
      in
        liftReturn (Msg idx) liftModel
          $ updateFn msg oldItem

