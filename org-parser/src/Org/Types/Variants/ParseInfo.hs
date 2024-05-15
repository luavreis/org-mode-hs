{-# LANGUAGE RecordWildCards #-}

module Org.Types.Variants.ParseInfo
  ( PropW (..)
  , OrgParsedF
  , OrgParsed
  , pattern OrgParsed
  , OrgObjectD
  , OrgObjects
  , OrgElementD
  , OrgElements
  , OrgSectionD
  , OrgSections

    -- * Constructors and patterns
  , object
  , pattern OrgObject
  , element
  , pattern OrgElement
  , section
  , pattern OrgSection

    -- * Re-exports
  , module Org.Types.Variants.Basic
  , module Org.Types.Data.StandardProperties
  ) where

import Data.Ix.RecursionSchemes (Fix (..))
import Data.Sequence (ViewL (..), ViewR (..), singleton, viewl, viewr, (|>))
import Org.Types.Data.StandardProperties
import Org.Types.Variants.Basic

data PropW t = PropW {props :: !StandardProperties, get :: !t}
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, NFData)

type OrgParsedF k ix = PropW (OrgF k ix)

type OrgParsed = Org (Compose (Seq :+: PropW))

pattern OrgParsed :: Seq (OrgParsedF OrgParsed i) -> OrgParsed i
pattern OrgParsed x = Org (Compose (Compose x))
{-# COMPLETE OrgParsed #-}

-- * Objects

pattern OrgObject :: Int -> Int -> OrgObjectData k ObjIx -> OrgParsedF k ObjIx
pattern OrgObject {begin, end, datum} = PropW (StandardProperties {postBlank = 0, ..}) (OrgObjectF datum)

object :: Int -> Int -> OrgObjectD -> OrgParsed ObjIx
object begin end datum = OrgParsed $ singleton $ OrgObject {..}

type OrgObjectD = OrgObjectData OrgParsed ObjIx
type OrgObjects = OrgParsed ObjIx

instance Semigroup OrgObjects where
  (OrgParsed xs) <> (OrgParsed ys) =
    OrgParsed $
      case (viewr xs, viewl ys) of
        (EmptyR, _) -> ys
        (_, EmptyL) -> xs
        (xs' :> x'@(PropW posx x), y'@(PropW posy y) :< ys') -> meld <> ys'
          where
            merged = PropW (StandardProperties posx.begin posy.end 0)
            meld =
              case (x, y) of
                (OrgObjectF (Plain t1), OrgObjectF (Plain t2))
                  | posx.end == posy.begin ->
                      xs' |> merged (OrgObjectF (Plain (t1 <> t2)))
                _other -> xs' |> x' |> y'

instance Monoid OrgObjects where
  mempty = coerce $ mempty @(Seq (OrgParsedF OrgParsed ObjIx))

-- * Elements

pattern OrgElement :: Int -> Int -> Int -> Keywords (k ObjIx) -> OrgElementData k ElmIx -> OrgParsedF k ElmIx
pattern OrgElement {begin, end, postBlank, keywords, datum} = PropW (StandardProperties {..}) (OrgElementF keywords datum)

element :: Int -> Int -> Int -> Keywords OrgObjects -> OrgElementD -> OrgElements
element begin end postBlank keywords datum = coerce $ singleton $ OrgElement {..}

type OrgElementD = OrgElementData OrgParsed ElmIx
type OrgElements = OrgParsed ElmIx
deriving newtype instance Semigroup OrgElements
deriving newtype instance Monoid OrgElements

-- * Sections

pattern OrgSection :: Int -> Int -> OrgSectionData k SecIx -> OrgParsedF k SecIx
pattern OrgSection {begin, end, datum} = PropW (StandardProperties {postBlank = 0, ..}) (OrgSectionF datum)

section :: Int -> Int -> OrgSectionD -> OrgSections
section begin end datum = coerce $ singleton $ OrgSection {..}

type OrgSectionD = OrgSectionData OrgParsed SecIx
type OrgSections = OrgParsed SecIx
deriving newtype instance Semigroup OrgSections
deriving newtype instance Monoid OrgSections
