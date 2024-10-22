module DOM.Virtual.App
  ( App
  , newApp
  , mountApp
  , rerenderApp
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.DOM (Element)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Node (appendChild)

import DOM.Virtual (VTree, createElement, patchElement, diff)

type DOMState =
  { tree :: VTree
  , elem :: Element
  }

newtype App = App (Ref DOMState)

newApp ::
  VTree ->
  Effect App
newApp tree = do
  elem <- createAppElement tree
  App <$> createDomRef tree elem

mountApp ::
  String ->
  App ->
  Effect (Maybe String)
mountApp containerId (App domRef) = do
  win <- window
  htmlDoc <- document win

  let parent = toNonElementParentNode $ toDocument htmlDoc

  container <- getElementById containerId parent

  case container of
    Nothing -> pure $ Just $ joinWith " "
      [ "DOM.Virtual.App.mountApp:"
      , "Couldn't find element with id"
      , containerId
      , "to mount the app."
      ]

    Just containerElem -> do
      domState <- Ref.read domRef
      _ <- appendChild (toNode domState.elem)
        (toNode containerElem)
      pure Nothing

rerenderApp ::
  VTree ->
  App ->
  Effect Unit
rerenderApp newTree (App domRef) = do
  domState <- Ref.read domRef

  let patches = diff domState.tree newTree

  newRoot <- patchElement patches domState.elem
  Ref.write { tree: newTree, elem: newRoot } domRef

-- Function aliases to sort out the effect types
-- of the actors involved
--
createAppElement ::
  VTree ->
  Effect Element
createAppElement tree = createElement tree

createDomRef ::
  VTree ->
  Element ->
  Effect (Ref DOMState)
createDomRef tree elem = Ref.new { tree, elem }

