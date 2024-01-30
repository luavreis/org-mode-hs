{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Variants.ParseInfo
  ( OrgF (..)
  , Org
  , OrgObjectD
  , OrgObjects
  , OrgElementD
  , OrgElements
  , OrgSectionD
  , OrgSections
  , OrgDocument

    -- * Constructors and patterns
  , object
  , pattern OrgObject
  , element
  , pattern OrgElement
  , section
  , pattern OrgSection

    -- * Re-exports
  , module Org.Types.Data.Document
  , module Org.Types.Data.Section
  , module Org.Types.Data.Element
  , module Org.Types.Data.Object
  , module Org.Types.Data.StandardProperties
  , module Org.Types.Data.Timestamp
  , module Org.Types.Ix
  ) where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Instances
import Data.Ix.RecursionSchemes (Fix (..))
import Data.Sequence (ViewL (..), ViewR (..), singleton, viewl, viewr, (|>))
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Document
import Org.Types.Data.Element
import Org.Types.Data.Object
import Org.Types.Data.Section
import Org.Types.Data.StandardProperties
import Org.Types.Data.Timestamp
import Org.Types.Ix
import Org.Types.Variants.Plain qualified as P

data OrgF k ix = OrgF {props :: StandardProperties, datum :: P.OrgF k ix}
  deriving (Generic)

deriving instance (AllOrgIx Eq k) => (Eq (OrgF k a))
deriving instance (AllOrgIx Ord k) => (Ord (OrgF k a))
deriving instance (AllOrgIx Show k) => (Show (OrgF k a))
deriving instance (AllOrgIx NFData k) => (NFData (OrgF k ix))

$(deriveGenericK ''OrgF)
deriving via (Generically OrgF) instance (Endofunctor (~>) OrgF)

type Org = Fix (ComposeIx Seq OrgF)

-- * Objects

pattern OrgObject :: Int -> Int -> OrgObjectData k ObjIx -> OrgF k ObjIx
pattern OrgObject {begin, end, datum} = OrgF (StandardProperties {postBlank = 0, ..}) (P.OrgObjectF datum)

object :: Int -> Int -> OrgObjectD -> Org ObjIx
object begin end datum = coerce $ singleton $ OrgObject {..}

type OrgObjectD = OrgObjectData Org ObjIx
type OrgObjects = Org ObjIx

instance Semigroup OrgObjects where
  (Fix (ComposeIx xs)) <> (Fix (ComposeIx ys)) =
    coerce
      $ case (viewr xs, viewl ys) of
        (EmptyR, _) -> ys
        (_, EmptyL) -> xs
        (xs' :> x'@(OrgF posx x), y'@(OrgF posy y) :< ys') -> meld <> ys'
          where
            merged = OrgF (StandardProperties posx.begin posy.end 0)
            meld =
              case (x, y) of
                (P.OrgObjectF (Plain t1), P.OrgObjectF (Plain t2))
                  | posx.end == posy.begin ->
                      xs' |> merged (P.OrgObjectF (Plain (t1 <> t2)))
                _ -> xs' |> x' |> y'

instance Monoid OrgObjects where
  mempty = coerce $ mempty @(Seq (OrgF Org ObjIx))

-- * Elements

pattern OrgElement :: Int -> Int -> Int -> Keywords (k ObjIx) -> OrgElementData k ElmIx -> OrgF k ElmIx
pattern OrgElement {begin, end, postBlank, keywords, datum} = OrgF (StandardProperties {..}) (P.OrgElementF keywords datum)

element :: Int -> Int -> Int -> Keywords OrgObjects -> OrgElementData Org ElmIx -> OrgElements
element begin end postBlank keywords datum = coerce $ singleton $ OrgElement {..}

type OrgElementD = OrgElementData Org ElmIx
type OrgElements = Org ElmIx
deriving newtype instance Semigroup OrgElements
deriving newtype instance Monoid OrgElements

-- * Sections

pattern OrgSection :: Int -> Int -> OrgSectionData k SecIx -> OrgF k SecIx
pattern OrgSection {begin, end, datum} = OrgF (StandardProperties {postBlank = 0, ..}) (P.OrgSectionF datum)

section :: Int -> Int -> OrgSectionData Org SecIx -> OrgSections
section begin end datum = coerce $ singleton $ OrgSection {..}

type OrgSectionD = OrgSectionData Org SecIx
type OrgSections = Org SecIx
deriving newtype instance Semigroup OrgSections
deriving newtype instance Monoid OrgSections

type OrgDocument = OrgDocumentData Org ElmIx
