module DOM.Erumu.Types
  ( UpdateFn
  , UpdateResult
  , Return(..)

  , withCommands
  , (!)
  , addCommands
  , mapModel
  , applyModel
  , mapMsg
  , liftUpdate
  , liftUpdateAnd
  , liftContinue
  , liftContinueAnd
  , liftReturn
  , liftReturnAnd
  , liftIntoUpdate
  , liftIntoUpdateAnd
  , updateResultFrom

  , SignalCommandEmitter
  , emitNone

  , HTML
  , element
  , text
  , texts
  , noElement
  , toVTree

  , Prop
  , attribute
  , hookProp
  , onEvent
  , onPreventDefaultEvent
  , onPropagatingEvent
  , onPropagatingNonMultiplePointerEvent
  , onEventDecode
  , onEventMaybeDecode

  , Command
  , DispatchFn
  , dispatchCmd
  , runCommand
  , nocmd
  , sendMsg
  , runAndSend
  , seqCmd
  , mapCmd
  , runcmd
  , hoistCommand
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe)
import Data.String as String
import Data.Foldable (fold, traverse_)
import Effect (Effect)

import DOM.Erumu.Decode (Decode)
import DOM.Erumu.Decode as Decode
import Web.Event.Event (Event)
import DOM.Virtual (Attribute, EventHandler, VTree, node, nonPropagatingEventHandler, preventDefaultEventHandler, propagatingEventHandler, propagatingNonMultiplePointerEventHandler, stringValue)
import DOM.Virtual as Virtual

--not sure about this. I actually think VTree shouldn't have a type parameter. I certainly don't want to add one for `HTML`. Maybe it should just be concrete.
newtype HTML msg = HTML (DispatchFnEff msg -> VTree)
newtype Prop msg = Prop (DispatchFnEff msg -> Attribute)
newtype Command m msg = Command (DispatchFn m msg -> m Unit)

type DispatchFn m msg = msg -> m Unit
type DispatchFnEff msg = msg -> Effect Unit

type UpdateResult m model msg = { model :: model, command :: Command m msg }
type UpdateFn m model msg = msg -> model -> UpdateResult m model msg

data Return m model msg signal
  = Continue (UpdateResult m model msg)
  | Return signal (UpdateResult m model msg)

-- applied functions will only lift the signal
instance functorReturn :: Functor (Return m model msg) where
  map f (Return signal update) = Return (f signal) update
  map _f (Continue update) = Continue update

withCommands ::
  forall m model msg.
  Applicative m =>
  model ->
  Array (Command m msg) ->
  UpdateResult m model msg
withCommands model commands = { model: model, command: fold commands }

-- Precedence set to 5 to allow combinations with $ like
--
--   Right $ m ! []
--
-- To work as desired
infixl 5 withCommands as !

addCommands ::
  forall m model msg.
  Applicative m =>
  UpdateResult m model msg ->
  Array (Command m msg) ->
  UpdateResult m model msg
addCommands update commands =
  { model: update.model
  , command: update.command <> fold commands
  }

mapModel ::
  forall m msg modelA modelB.
  (modelA -> modelB) ->
  UpdateResult m modelA msg ->
  UpdateResult m modelB msg
mapModel f update = update { model = f update.model }

applyModel ::
  forall m msg modelA modelB.
  Apply m =>
  UpdateResult m (modelA -> modelB) msg ->
  UpdateResult m modelA msg ->
  UpdateResult m modelB msg
applyModel updateF updateA =
  { model: updateF.model updateA.model
  , command: updateF.command <> updateA.command
  }

mapMsg ::
  forall m model msgA msgB.
  (msgA -> msgB) ->
  UpdateResult m model msgA ->
  UpdateResult m model msgB
mapMsg f update = update { command = f <$> update.command }

liftUpdate ::
  forall m model msg parentModel parentMsg.
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  (UpdateResult m model msg) ->
  (UpdateResult m parentModel parentMsg)
liftUpdate liftMsg liftModel update = liftUpdateAnd liftMsg liftModel identity update

liftUpdateAnd ::
  forall m model msg parentModel parentMsg.
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  (parentModel -> parentModel) ->
  (UpdateResult m model msg) ->
  (UpdateResult m parentModel parentMsg)
liftUpdateAnd liftMsg liftModel modelFn update =
  { model: modelFn $ liftModel update.model
  , command: liftMsg <$> update.command
  }

type SignalCommandEmitter m signal msg = (signal -> Array (Command m msg))

emitNone :: forall m signal msg. SignalCommandEmitter m signal msg
emitNone = const []

-- For parents just lifting the Return (of the same signal type) as their child
liftReturn ::
  forall m model msg signal parentMsg parentModel.
  Applicative m =>
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  Return m model msg signal ->
  Return m parentModel parentMsg signal
liftReturn liftMsg liftModel =
  liftReturnAnd liftMsg liftModel (const identity) emitNone

-- For parents who want side effects of processing the Return (of the same signal type)..
-- This applies the "and" `modifyModel` and `emitter` functions only on `Return` cases
liftReturnAnd ::
  forall m model msg signal parentMsg parentModel.
  Applicative m =>
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  (signal -> parentModel -> parentModel) ->
  SignalCommandEmitter m signal parentMsg ->
  Return m model msg signal ->
  Return m parentModel parentMsg signal
liftReturnAnd liftMsg liftModel modifyModel emitter r =
  case r of
    Continue upd -> Continue $ liftUpdate liftMsg liftModel upd
    Return v upd -> Return v $ addCommands (liftUpdateAnd liftMsg liftModel (modifyModel v) upd) (emitter v)

updateResultFrom ::
  forall m signal model msg.
  Return m model msg signal ->
  UpdateResult m model msg
updateResultFrom r =
  case r of
    Return _val upd -> upd
    Continue upd -> upd

-- for parents that quietly return UpdateResult m
liftIntoUpdate ::
  forall m model msg signal parentMsg parentModel.
  Applicative m =>
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  Return m model msg signal ->
  UpdateResult m parentModel parentMsg
liftIntoUpdate liftMsg liftModel =
  liftIntoUpdateAnd liftMsg liftModel (const identity) emitNone

-- for parents that return UpdateResult m with side effects
liftIntoUpdateAnd ::
  forall m model msg signal parentMsg parentModel.
  Applicative m =>
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  (signal -> parentModel -> parentModel) ->
  SignalCommandEmitter m signal parentMsg ->
  Return m model msg signal ->
  UpdateResult m parentModel parentMsg
liftIntoUpdateAnd liftMsg liftModel modifyModel emitter r =
  updateResultFrom
    $ liftReturnAnd liftMsg liftModel modifyModel emitter r

-- For parents that want to ignore their child's Return and quietly lift-then-Continue
liftContinue ::
  forall m model msg childSignal parentSignal parentMsg parentModel.
  Applicative m =>
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  Return m model msg childSignal ->
  Return m parentModel parentMsg parentSignal
liftContinue liftMsg liftModel r =
  Continue $ liftIntoUpdate liftMsg liftModel r

-- For parents that want to quietly lift-then-Continue after processing the child Return
liftContinueAnd ::
  forall m model msg childSignal parentSignal parentMsg parentModel.
  Applicative m =>
  (msg -> parentMsg) ->
  (model -> parentModel) ->
  (childSignal -> parentModel -> parentModel) ->
  SignalCommandEmitter m childSignal parentMsg ->
  Return m model msg childSignal ->
  Return m parentModel parentMsg parentSignal
liftContinueAnd liftMsg liftModel modifyModel emitter r =
  Continue $ liftIntoUpdateAnd liftMsg liftModel modifyModel emitter r

dispatchCmd :: forall m msg. (DispatchFn m msg -> m Unit) -> Command m msg
dispatchCmd = Command

runCommand :: forall m msg. DispatchFn m msg -> Command m msg -> m Unit
runCommand f (Command c) =
  c f

hoistCommand ::
  forall m f msg.
  (m ~> f) ->
  (f ~> m) ->
  Command m msg ->
  Command f msg
hoistCommand toF toM (Command mAction) =
  Command $ \dispatchF ->
    toF (mAction (toM <<< dispatchF))

nocmd :: forall m msg. Applicative m => Command m msg
nocmd = Command (\_ -> pure unit)

-- Instead of:

--   dispatchCmd $ \dispatch -> do
--     ...
--     dispatch x

-- You can use:

--   runAndSend $ do
--     ...
--     pure x
runAndSend :: forall m msg. Bind m => m msg -> Command m msg
runAndSend action = Command (action >>= _)

sendMsg :: forall m msg. msg -> Command m msg
sendMsg msg = Command (\dispatch -> dispatch msg)

seqCmd ::
  forall m msg.
  Apply m =>
  Command m msg ->
  Command m msg ->
  Command m msg
seqCmd (Command c1) (Command c2) =
  Command c1c2
  where
  c1c2 d = c1 d *> c2 d

mapCmd ::
  forall m msg1 msg2.
  (msg1 -> msg2) ->
  Command m msg1 ->
  Command m msg2
mapCmd f (Command action) = Command (action <<< liftMessages f)

instance functorCommand :: Functor (Command m) where
  map = mapCmd

instance semigroupCommand :: Apply m => Semigroup (Command m msg) where
  append = seqCmd

instance monoidCommand :: Applicative m => Monoid (Command m msg) where
  mempty = nocmd

liftMessages ::
  forall m msg1 msg2.
  (msg1 -> msg2) ->
  DispatchFn m msg2 ->
  DispatchFn m msg1
liftMessages f g = g <<< f

runcmd ::
  forall m msg.
  Command m msg ->
  DispatchFn m msg ->
  (m Unit -> Effect Unit) ->
  Effect Unit
runcmd (Command f) dispatch execEff = execEff (f dispatch)

instance functorProp :: Functor Prop where
  map f (Prop go) = Prop (go <<< liftMessages f)

instance functorHTML :: Functor HTML where
  map f (HTML go) = HTML (go <<< liftMessages f)

text :: forall msg. String -> HTML msg
text t = HTML (\_ -> Virtual.text t)

texts :: forall msg. Array String -> HTML msg
texts = text <<< String.joinWith " "

noElement :: forall msg. HTML msg
noElement = text ""

element :: forall msg. String -> Array (Prop msg) -> Array (HTML msg) -> HTML msg
element name props dsls =
  HTML render
  where
  render :: DispatchFnEff msg -> VTree
  render disp =
    let
      attrs = map (toAttrR disp) props
      trees = map (toVTreeR disp) dsls
    in
      node name attrs trees

hookProp :: forall msg. String -> Virtual.Hook -> Prop msg
hookProp name hook =
  Prop (\_ -> { key: name, value: Virtual.hookValue hook })

onEvent :: forall msg. String -> msg -> Prop msg
onEvent name dat =
  handlerProp name
    nonPropagatingEventHandler
    (\_ -> pure [ dat ])

onPropagatingEvent :: forall msg. String -> msg -> Prop msg
onPropagatingEvent name dat =
  handlerProp name
    propagatingEventHandler
    (\_ -> pure [ dat ])

onPropagatingNonMultiplePointerEvent :: forall msg. String -> msg -> Prop msg
onPropagatingNonMultiplePointerEvent name dat =
  handlerProp name
    propagatingNonMultiplePointerEventHandler
    (\_ -> pure [ dat ])

onPreventDefaultEvent :: forall msg. String -> msg -> Prop msg
onPreventDefaultEvent name dat =
  handlerProp name
    preventDefaultEventHandler
    (\_ -> pure [ dat ])

-- This event decode property is used to declare a handler that will *always* process the event into
-- a Schmods Msg to dispatch, and stops propagation to potential parent handlers.
onEventDecode :: forall msg. String -> (Decode msg) -> Prop msg
onEventDecode name decode =
  handlerProp name
    nonPropagatingEventHandler
    (\event -> pure <$> Decode.runOrCrash decode event)

-- This event decode property is used to declare a handler that *may or may not* process the event
-- and it may (will) propagate the event to potential parent handlers.

-- Original use case was to allow `Esc` key presses to bubble up to close a wrapping modal
onEventMaybeDecode :: forall msg. String -> (Decode (Maybe msg)) -> Prop msg
onEventMaybeDecode name decode =
  handlerProp name
    propagatingEventHandler
    (\event -> catMaybes <$> pure <$> Decode.runOrCrash decode event)

handlerProp ::
  forall msg.
  String ->
  EventHandler ->
  (Event -> Effect (Array msg)) ->
  Prop msg
handlerProp name handler mkMsg =
  Prop
    ( \dispatch ->
        { key: name
        , value: handler (traverse_ dispatch <=< mkMsg)
        }
    )

attribute :: forall msg. String -> String -> Prop msg
attribute name value =
  Prop (\_ -> { key: name, value: stringValue value })

toAttr :: forall msg. Prop msg -> DispatchFnEff msg -> Attribute
toAttr (Prop attr) = attr

toAttrR :: forall msg. DispatchFnEff msg -> Prop msg -> Attribute
toAttrR app prop = toAttr prop app

toVTree :: forall msg. HTML msg -> DispatchFnEff msg -> VTree
toVTree (HTML tree) = tree

toVTreeR :: forall msg. DispatchFnEff msg -> HTML msg -> VTree
toVTreeR app dsl = toVTree dsl app

