module DOM.Erumu.Nodes
  ( toInputElement
  , toSelectElement
  , toTextAreaElement
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Web.HTML (HTMLInputElement, HTMLSelectElement, HTMLTextAreaElement)
import Web.DOM (Element, Node)
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.Element (tagName)
import Web.DOM.Node (nodeTypeIndex)
import Unsafe.Coerce (unsafeCoerce)

toInputElement :: Node -> Either String HTMLInputElement
toInputElement = unsafeToElement "INPUT"

toSelectElement :: Node -> Either String HTMLSelectElement
toSelectElement = unsafeToElement "SELECT"

toTextAreaElement :: Node -> Either String HTMLTextAreaElement
toTextAreaElement = unsafeToElement "TEXTAREA"

unsafeToElement ::
  forall elem.
  String ->
  Node ->
  Either String elem
unsafeToElement expectedTagName node =
  lmap (annotateError expectedTagName) $ do
    genericElem <- nodeToElement node

    if tagName genericElem == expectedTagName then Right $ unsafeCoerce genericElem
    else Left $ tagName genericElem

annotateError :: String -> String -> String
annotateError tagName err =
  "expected " <> tagName <> "element, but got: " <> err

nodeToElement :: Node -> Either String Element
nodeToElement node =
  if fromEnum ElementNode == nodeTypeIndex node then Right $ unsafeCoerce node
  else Left $ "Node type: " <> show (nodeTypeIndex node)

