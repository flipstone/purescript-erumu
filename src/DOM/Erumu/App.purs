module DOM.Erumu.App
  ( App
  , newApp
  , mountApp
  , dispatch
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)

import DOM.Erumu.Types (UpdateFn, HTML, runcmd, toVTree)
import DOM.Virtual (node)
import DOM.Virtual.App as Virtual

newtype App m model msg = App
  { update :: UpdateFn m model msg
  , render :: RenderFn model msg

  , execM :: m Unit -> Effect Unit
  , liftM :: Effect Unit -> m Unit

  , stRef :: Ref (DispatchState model msg)
  , virtualApp :: Virtual.App
  }

type DispatchState model msg =
  { model :: model
  , messages :: List msg
  , processing :: Boolean
  }

type RenderFn model msg = model -> HTML msg

--
-- newApp guarantees:
--
--  * The app is rendered and ready to mount when it is returned
--  * Messages are processed in the order dispatched, one at a time
--  * The dom is rendered and updated before Commands are run
--  * Dispatching a message is always "safe"
--    - e.g. you can save a dispatch function for later call it at any time.
--
newApp :: forall m model msg.
          (model -> HTML msg)
       -> UpdateFn m model msg
       -> model
       -> (m Unit -> Effect Unit)
      -> (Effect Unit -> m Unit)
       -> Effect (App m model msg)
newApp render update init execM liftM = do
  virtualApp <- Virtual.newApp (node "div" [] [])
  stRef <- Ref.new { model: init
                   , messages: Nil
                   , processing: false
                   }

  let app = App { update, render, stRef, virtualApp, execM, liftM }

  renderApp app
  pure app

mountApp :: forall m model msg.
            String
         -> App m model msg
         -> Effect (Maybe String)
mountApp containerId (App { virtualApp }) =
  Virtual.mountApp containerId virtualApp


dispatch :: forall m model msg.
            App m model msg
         -> msg
         -> Effect Unit
dispatch app@(App { stRef }) msg = do
  pushMessage stRef msg
  processMessages app

renderApp :: forall m model msg.
             App m model msg
          -> Effect Unit
renderApp app@(App { virtualApp, render, stRef }) = do
  st <- Ref.read stRef
  Virtual.rerenderApp (toVTree (render st.model) (dispatch app))
                       virtualApp

tryTakeMessages :: forall model msg.
                   Ref (DispatchState model msg)
                -> Effect (Maybe (List msg))
tryTakeMessages stRef =
  (flip Ref.modify') stRef $
    \st ->
      if st.processing
      then { state: st, value: Nothing }
      else { state: st { messages = Nil }
           , value: Just (reverse st.messages)
           }

setProcessing :: forall model msg.
                 Ref (DispatchState model msg)
              -> Boolean
              -> Effect Unit
setProcessing stRef processing = void $
  Ref.modify (_ { processing = processing }) stRef

pushMessage :: forall model msg.
               Ref (DispatchState model msg)
            -> msg
            -> Effect Unit
pushMessage stRef msg = void $
  Ref.modify (\st -> st { messages = Cons msg st.messages }) stRef

processMessages :: forall m msg model.
                   App m model msg
                -> Effect Unit
processMessages app@(App { stRef }) = void $ do
  taken <- tryTakeMessages stRef

  case taken of
    Nothing   -> pure unit -- messages are already being processed
    Just Nil  -> pure unit -- there are no messages to process
    Just msgs -> do
      setProcessing stRef true
      traverse_ (processMessage app) msgs
      setProcessing stRef false
      processMessages app

processMessage :: forall m msg model.
                  App m model msg
               -> msg
               -> Effect Unit
processMessage app@(App { update, stRef, execM, liftM }) msg = void $ do
  command <- (flip Ref.modify') stRef $ \st ->
    let r = update msg st.model
     in { state: st { model = r.model }
        , value: r.command
        }

  renderApp app
  runcmd command (liftM <<< dispatch app) execM

