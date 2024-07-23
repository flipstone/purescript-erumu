module DOM.Erumu.Identifier
  ( Identifier
  , base
  , str
  , id_
  , pred
  , succ
  ) where

import Prelude

import Data.Maybe (Maybe(Just))
import Data.Enum as Enum

import DOM.Erumu.HTML as HTML
import DOM.Erumu.Types (Prop)

type Sequence = Int

data Identifier = Identifier Sequence String

base :: String -> Identifier
base = Identifier 0

id_ :: forall msg. Identifier -> Prop msg
id_ = HTML.id_ <<< str

str :: Identifier -> String
str (Identifier s b) = b <> show s

-- Strictly speaking, succ could overflow (or pred could underflow) the
-- 32-bit integer range. However, because we do not expose operations to
-- either
--
--    a) construct Identifier sequence values with numbers other than 0
--  or
--    b) add (or substruct) any value greater than 1 from the sequence
--
-- in order to actually achieve the feat of overflowing succ would require
-- 2^31 (2,147,483,648) operations to be performed. If you are here because
-- you have managed to do this and have gotten bit by overflow, then I offer
-- both congratulations and sincere condolences.
--

pred :: Identifier -> Identifier
pred (Identifier s b) = Identifier (s - 1) b

succ :: Identifier -> Identifier
succ (Identifier s b) = Identifier (s + 1) b

instance Eq Identifier where
  eq (Identifier s1 b1) (Identifier s2 b2) = s1 == s2 && b1 == b2

instance Ord Identifier where
  compare (Identifier s1 b1) (Identifier s2 b2) =
    compare b1 b2 <> compare s1 s2

instance Enum.Enum Identifier where
  pred = Just <<< pred
  succ = Just <<< succ

