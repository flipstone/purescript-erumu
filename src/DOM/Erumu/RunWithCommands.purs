module DOM.Erumu.RunWithCommands
  ( RunWithCommands
  , run
  , lift
  , mapMsg
  , mapModel
  , applyModel
  , bindModel
  , pureModel
  , fromUpdate
  , fromCommand
  , toCommand
  , sendMsg
  , loadChildViaMsg
  ) where

import Prelude
import Control.Parallel as Par

import DOM.Erumu.Types (UpdateResult, Command, (!), dispatchCmd, runCommand, nocmd)
import DOM.Erumu.Types as Types

{-
  'RunWithCommands' can be used when you have an impure initializer of a widget
  that also needs to return an 'UpdateResult' that schedules side-effects to be
  run. This typically happens when a parent widget needs to make an impure call
  to initialize itself but also has children that return an 'UpdateResult' as
  from their init functions. The parent can use the 'fromUpdate' function found
  here along with 'mapMsg' to incorporate the child's initialization into its
  own and be sure that any commands request by the child are not accidentally
  dropped.
-}
newtype RunWithCommands m msg model =
  RunWithCommands (m (UpdateResult m model msg))

instance runWithCommandsFunctor :: Functor m => Functor (RunWithCommands m msg) where
  map = mapModel

instance runWithCommandsApply :: Apply m => Apply (RunWithCommands m msg) where
  apply = applyModel

instance runWithCommandsApplicative :: Applicative m => Applicative (RunWithCommands m msg) where
  pure = pureModel

instance runWithCommandsBind :: Monad m => Bind (RunWithCommands m msg) where
  bind = bindModel

instance runWithCommandsMonad :: Monad m => Monad (RunWithCommands m msg)

instance runWithCommandsParallel :: Par.Parallel f m => Par.Parallel (RunWithCommands f msg) (RunWithCommands m msg) where
  sequential = hoist Par.sequential Par.parallel
  parallel = hoist Par.parallel Par.sequential

hoist :: forall m f msg model.
         Functor f
      => (m ~> f)
      -> (f ~> m)
      -> RunWithCommands m msg model
      -> RunWithCommands f msg model
hoist toF toM (RunWithCommands action) =
  let
    hoistUpdate update =
      update
        { command = Types.hoistCommand toF toM update.command
        }
  in
    RunWithCommands $
      map hoistUpdate (toF action)

run :: forall m msg model.
       RunWithCommands m msg model
    -> m (UpdateResult m model msg)
run (RunWithCommands action) =
  action

lift :: forall m msg model.
        Applicative m
     => m model
     -> RunWithCommands m msg model
lift monad =
  RunWithCommands $
    map ({model: _, command: nocmd}) monad

mapModel :: forall msg m modelA modelB.
            Functor m
         => (modelA -> modelB)
         -> RunWithCommands m msg modelA
         -> RunWithCommands m msg modelB
mapModel f (RunWithCommands action) =
  RunWithCommands $
    map (Types.mapModel f) action

applyModel :: forall msg m modelA modelB.
              Apply m
           => RunWithCommands m msg (modelA -> modelB)
           -> RunWithCommands m msg modelA
           -> RunWithCommands m msg modelB
applyModel (RunWithCommands actionF) (RunWithCommands actionA) =
  RunWithCommands $
    map Types.applyModel actionF <*> actionA

bindModel :: forall msg m modelA modelB.
             Monad m
          => RunWithCommands m msg modelA
          -> (modelA -> RunWithCommands m msg modelB)
          -> RunWithCommands m msg modelB
bindModel (RunWithCommands actionA) f =
  RunWithCommands $ do
    updateA <- actionA
    updateB <- run (f updateA.model)
    pure $
      { model: updateB.model
      , command: updateA.command <> updateB.command
      }

pureModel :: forall msg m model.
             Applicative m
          => model
          -> RunWithCommands m msg model
pureModel model =
  RunWithCommands (pure $ model ! [])

mapMsg :: forall msgA msgB m model.
          Functor m
       => (msgA -> msgB)
       -> RunWithCommands m msgA model
       -> RunWithCommands m msgB model
mapMsg f (RunWithCommands action) =
  RunWithCommands $
    map (Types.mapMsg f) action

fromUpdate :: forall m msg model.
              Applicative m
           => UpdateResult m model msg
           -> RunWithCommands m msg model
fromUpdate update =
  RunWithCommands (pure update)

fromCommand :: forall m msg.
               Applicative m
            => Command m msg
            -> RunWithCommands m msg Unit
fromCommand command =
  RunWithCommands $
    pure
      { model: unit
      , command: command
      }

toCommand :: forall m msg.
             Bind m
          => RunWithCommands m msg Unit
          -> Command m msg
toCommand (RunWithCommands action) =
  dispatchCmd $ \dispatch -> do
    update <- action
    runCommand dispatch update.command

sendMsg :: forall m msg.
           Applicative m
        => msg
        -> RunWithCommands m msg Unit
sendMsg msg =
  fromCommand (dispatchCmd (\dispatch -> dispatch msg))

{-
  Run a child's init within a parent's update context and dispatch a message to
  tell the parent the child is loaded. This function ensures that the parents
  load message is dispatched _before_ any command that the child init wants to
  run so that any messages dispatched by the child will be delivery after the
  load message  has been delivered to the parent.
-}
loadChildViaMsg :: forall m parentMsg childMsg childModel.
                   Monad m
                => (childModel -> parentMsg)
                -> (childMsg -> parentMsg)
                -> RunWithCommands m childMsg childModel
                -> RunWithCommands m parentMsg Unit
loadChildViaMsg mkLoadMsg wrapChildMsg (RunWithCommands action) =
  RunWithCommands $ do
    childUpdate <- action

    let
      sendLoadMsg =
        dispatchCmd (\dispatch -> dispatch (mkLoadMsg childUpdate.model))

    pure
      { model: unit
      , command: sendLoadMsg <> map wrapChildMsg childUpdate.command
      }
