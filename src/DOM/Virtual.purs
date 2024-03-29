module DOM.Virtual
  ( Value, VTree, Diff, Attribute, Attributes
  , createElement, patchElement, diff

  , node, text

  , Hook, HookFunctions, newHook, hookValue

  , EventHandler
  , unsafeValue, stringValue, propagatingEventHandler, nonPropagatingEventHandler
  ) where

import Prelude

import Effect (Effect)
import Web.Event.Event (Event)
import Web.DOM (Node, Element)

foreign import data Value :: Type
foreign import data VTree :: Type
foreign import data Diff :: Type

type Attribute = { key :: String, value :: Value }
type Attributes = Array Attribute

foreign import diff :: VTree -> VTree -> Diff
foreign import createElement :: VTree -> Effect Element
foreign import patchElement :: Diff -> Element -> Effect Element

foreign import node :: String
                    -> Attributes
                    -> Array VTree
                    -> VTree

foreign import text :: String -> VTree

foreign import unsafeValue :: forall v. v -> Value

--
-- Some cases (most app controls) we want to stop propagation when a handler has been declared
-- Some cases (mainly highly-nested form fields) we want to pass the events up (like an Esc key)
--
type EventHandler = (Event -> Effect Unit) -> Value

foreign import nonPropagatingEventHandler :: EventHandler
foreign import propagatingEventHandler :: EventHandler

foreign import data HookFn :: Type
foreign import hookFn :: (Node -> Effect Unit) -> HookFn

stringValue :: String -> Value
stringValue = unsafeValue

newtype Hook = Hook { __proto__ :: { hook :: HookFn, unhook :: HookFn } }

type HookFunctions =
  { hook :: Node -> Effect Unit
  , unhook :: Node -> Effect Unit
  }

-- Constructs a Hook that can be held on to for use as a property value
-- later. The virtual-dom library uses object equality to detect hook
-- changes to trigger calls to these hooks, to it's important to keep
-- use the same hook instance on each call to render in order to *not*
-- have your hook functions spammed with calls.
newHook :: HookFunctions -> Hook
newHook { hook, unhook } =
  Hook { __proto__: { hook: hookFn hook, unhook: hookFn unhook } }

hookValue :: Hook -> Value
hookValue (Hook e) = unsafeValue e
