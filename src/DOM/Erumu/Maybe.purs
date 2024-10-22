module DOM.Erumu.Maybe
  ( update
  , updateF
  , maybeReturn
  ) where

import Prelude
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)

import DOM.Erumu.Types (UpdateResult, Return(..), liftUpdate, (!))

-- This is for when you need to generate an UpdateResult for a model that is
-- nested inside a maybe. It takes care of pushing the Maybe back down to the
-- model rather than ending up with (Maybe (UpdateResult ...))
--
update ::
  forall m msg model.
  Applicative m =>
  (model -> UpdateResult m model msg) ->
  Maybe model ->
  UpdateResult m (Maybe model) msg
update f = unwrap <<< updateF (Identity <<< f)

updateF ::
  forall f m msg model.
  Applicative f =>
  Applicative m =>
  (model -> f (UpdateResult m model msg)) ->
  Maybe model ->
  f (UpdateResult m (Maybe model) msg)
updateF _ Nothing = pure (Nothing ! [])
updateF updateModel (Just model) =
  liftUpdate identity Just <$> updateModel model

-- Conditionally update and lift a Maybe childmodel.
-- Continue if the child is Nothing
maybeReturn ::
  forall m msg model parentModel parentMsg childSignal parentSignal.
  Applicative m =>
  parentModel ->
  (Return m model msg childSignal -> Return m parentModel parentMsg parentSignal) ->
  (model -> (Return m model msg childSignal)) ->
  Maybe model ->
  Return m parentModel parentMsg parentSignal
maybeReturn parentModel liftFn updateFn model =
  maybe (Continue $ parentModel ! []) (liftFn <<< updateFn) model
