module DOM.Erumu.Either
  ( updateLeft
  , updateRight
  ) where

import Prelude

import Data.Either (Either(Left, Right))

import DOM.Erumu.Types (UpdateResult, liftUpdate, (!))

updateLeft ::
  forall m msg model a.
  Applicative m =>
  (model -> UpdateResult m model msg) ->
  Either model a ->
  UpdateResult m (Either model a) msg
updateLeft _ right@(Right _) =
  right ! []

updateLeft f (Left model) =
  liftUpdate identity Left (f model)

updateRight ::
  forall m msg model a.
  Applicative m =>
  (model -> UpdateResult m model msg) ->
  Either a model ->
  UpdateResult m (Either a model) msg
updateRight _ left@(Left _) =
  left ! []

updateRight f (Right model) =
  liftUpdate identity Right (f model)
