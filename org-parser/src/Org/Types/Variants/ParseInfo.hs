{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Variants.ParseInfo
  ( OrgF (..)
  , Org
  , OrgObjects
  , OrgElements
  , OrgSections
  , OrgDocument

    -- * Pos
  , Position (..)

    -- * Re-exports
  , module Org.Types.Data.Document
  , module Org.Types.Data.Section
  , module Org.Types.Data.Element
  , module Org.Types.Data.Object
  , module Org.Types.Data.Timestamp
  ) where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Instances
import Data.Ix.RecursionSchemes (Fix (..))
import Data.Sequence (ViewL (..), ViewR (..), viewl, viewr, (|>))
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Document
import Org.Types.Data.Element
import Org.Types.Data.Object
import Org.Types.Data.Section
import Org.Types.Data.Timestamp
import Org.Types.Ix
import Org.Types.Variants.Plain qualified as P

data Position = Position
  { begin :: !Int
  , end :: !Int
  }
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

data OrgF k ix = OrgF {pos :: Position, datum :: P.OrgF k ix}

$(deriveGenericK ''OrgF)
deriving via (Generically OrgF) instance (Endofunctor (~>) OrgF)

type Org = Fix (ComposeIx Seq OrgF)

type OrgObjects = Org ObjIx

instance Semigroup OrgObjects where
  (Fix (ComposeIx xs)) <> (Fix (ComposeIx ys)) =
    coerce
      $ case (viewr xs, viewl ys) of
        (EmptyR, _) -> ys
        (_, EmptyL) -> xs
        (xs' :> x'@(OrgF posx x), y'@(OrgF posy y) :< ys') -> meld <> ys'
          where
            merged = OrgF (Position posx.begin posy.end)
            meld =
              case (x, y) of
                (P.OrgObjectF (Plain t1), P.OrgObjectF (Plain t2))
                  | posx.end == posy.begin ->
                      xs' |> merged (P.OrgObjectF (Plain (t1 <> t2)))
                _ -> xs' |> x' |> y'

instance Monoid OrgObjects where
  mempty = coerce $ mempty @(Seq (OrgF Org ObjIx))

type OrgElements = Org ElmIx
deriving newtype instance Semigroup OrgElements
deriving newtype instance Monoid OrgElements

type OrgSections = Org SecIx
type OrgDocument = OrgDocumentData Org ElmIx
