{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Org.Builder where

import Data.Data (Data)
import Data.Sequence (ViewL (..), ViewR (..), viewl, viewr, (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Exts qualified
import Org.Types

newtype Many a = Many {unMany :: Seq a}
  deriving (Eq, Ord, Read, Show, Typeable, Functor, Foldable, Traversable, Data, Generic)
  deriving anyclass (NFData)

instance One (Many a) where
  type OneItem (Many a) = a
  one = Many . Seq.singleton

instance IsList (Many a) where
  type Item (Many a) = a
  fromList = Many . fromList
  toList = toList . (.unMany)

type OrgObjects = Many OrgObjectB

type OrgObjectBD = OrgObjectData OrgObjects

data OrgObjectB = OrgObjectB
  { pos :: Pos
  , object :: OrgObjectBD
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type OrgElements = Many OrgElementB

type OrgElementBD = OrgElementData OrgObjects OrgElements

-- | Org element. Like a Pandoc Block.
data OrgElementB = OrgElementB
  { pos :: Pos
  , affKeywords :: Keywords OrgObjects
  , element :: OrgElementBD
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

deriving newtype instance Semigroup OrgElements

deriving newtype instance Monoid OrgElements

instance Semigroup OrgObjects where
  (Many xs) <> (Many ys) =
    case (viewr xs, viewl ys) of
      (EmptyR, _) -> Many ys
      (_, EmptyL) -> Many xs
      (xs' :> x'@(OrgObjectB posx x), y'@(OrgObjectB posy y) :< ys') ->
        Many (meld <> ys')
        where
          merged = OrgObjectB (Pos posx.begin posy.end)
          meld =
            case (x, y) of
              (Plain t1, Plain t2)
                | posx.end == posy.begin ->
                    xs' |> merged (Plain (t1 <> t2))
              _ -> xs' |> x' |> y'

instance Monoid OrgObjects where
  mempty = Many mempty

instance IsString OrgObjectBD where
  fromString = Plain . T.pack

-- * Element builders

element :: Pos -> [(Text, KeywordValue OrgObjects)] -> OrgElementBD -> OrgElements
element pos aff = one . OrgElementB pos (fromList aff)

listItemUnord :: Char -> OrgElements -> ListItem OrgObjects OrgElements
listItemUnord s = ListItem (Bullet s) Nothing Nothing Nothing

orderedList ::
  OrderedStyle ->
  Char ->
  [OrgElements] ->
  OrgElementBD
orderedList style separator =
  PlainList (Ordered style)
    . zipWith (\b -> ListItem b Nothing Nothing Nothing) bullets
  where
    bullets = case style of
      OrderedNum -> [Counter (show i) separator | i :: Int <- [1 ..]]
      OrderedAlpha -> [Counter (one a) separator | a <- ['a' ..]]

descriptiveList :: [(OrgObjects, OrgElements)] -> OrgElementBD
descriptiveList =
  PlainList Descriptive
    . map (\(tag, els) -> ListItem (Bullet '-') Nothing Nothing (Just tag) els)

-- * Object builders

object :: Pos -> OrgObjectBD -> OrgObjects
object pos = one . OrgObjectB pos
