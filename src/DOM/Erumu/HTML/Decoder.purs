module DOM.Erumu.HTML.Decoder
  ( decodeTarget
  , Target

  , inputTarget
  , inputValue
  , inputChecked
  , inputFiles

  , selectTarget
  , selectValue
  , selectedIndex

  , textAreaTarget
  , textAreaValue
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import DOM.Erumu.Decode (Decode, domValue)
import DOM.Erumu.Decode as Decode
import DOM.Erumu.Nodes as Nodes
import Web.Event.Event (currentTarget)
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLSelectElement as Select
import Web.HTML.HTMLTextAreaElement as TextArea
import Web.HTML (HTMLInputElement, HTMLSelectElement, HTMLTextAreaElement)
import Web.DOM (Node)
import Web.DOM.Node (fromEventTarget)
import Web.File.FileList (FileList)

data Target a = Target String (Node -> Either String a)

decodeTarget :: forall a. Target a -> Decode a
decodeTarget (Target description toElement) = do
  mbTarget <- currentTarget <$> Decode.event
  let mbNode = join $ fromEventTarget <$> mbTarget

  case toElement <$> mbNode of
    (Just (Right elem)) -> pure elem
    (Just (Left err)) -> Decode.crash ("decodeTarget: " <> description <> ": " <> err)
    Nothing -> Decode.crash ("decodeTarget: no node")

--
-- Decoding input elements
--
inputTarget :: Target HTMLInputElement
inputTarget = Target "input" Nodes.toInputElement

inputValue :: Decode String
inputValue = domValue Input.value =<< decodeTarget inputTarget

inputChecked :: Decode Boolean
inputChecked = domValue Input.checked =<< decodeTarget inputTarget

inputFiles :: Decode (Maybe FileList)
inputFiles = domValue Input.files =<< decodeTarget inputTarget

--
-- Decoding select elements
--
selectTarget :: Target HTMLSelectElement
selectTarget = Target "select" Nodes.toSelectElement

selectValue :: Decode String
selectValue = domValue Select.value =<< decodeTarget selectTarget

selectedIndex :: Decode Int
selectedIndex = domValue Select.selectedIndex =<< decodeTarget selectTarget

--
-- Decoding textarea elements
--
textAreaTarget :: Target HTMLTextAreaElement
textAreaTarget = Target "textarea" Nodes.toTextAreaElement

textAreaValue :: Decode String
textAreaValue = domValue TextArea.value =<< decodeTarget textAreaTarget

