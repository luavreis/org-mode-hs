{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Org.Types.Variants.Annotated
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
  , pattern OrgObject'
  , element
  , pattern OrgElement
  , pattern OrgElement'
  , section
  , pattern OrgSection
  , pattern OrgSection'

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
import Data.Aeson.Types (Object)
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.RecursionSchemes (Fix (..))
import Data.Ix.Traversable (ITraversable)
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Document
import Org.Types.Data.Element
import Org.Types.Data.Object
import Org.Types.Data.Section
import Org.Types.Data.StandardProperties
import Org.Types.Data.Timestamp
import Org.Types.Ix
import Org.Types.Variants.Plain qualified as P

data OrgF k ix = OrgF {props :: StandardProperties, annotations :: Object, datum :: P.OrgF k ix}
  deriving (Typeable, Generic)

instance TestIxEq (OrgF k) where
  testIxEq x y = testIxEq x y.datum

deriving instance (Eq (P.OrgF k a)) => (Eq (OrgF k a))
deriving instance (Ord (P.OrgF k a)) => (Ord (OrgF k a))
deriving instance (Show (P.OrgF k a)) => (Show (OrgF k a))
deriving instance (NFData (P.OrgF k a)) => (NFData (OrgF k a))

$(deriveGenericK ''OrgF)
deriving via (Generically OrgF) instance (Endofunctor (~>) OrgF)
deriving via (Generically OrgF) instance (IFoldable OrgF)
deriving via (Generically OrgF) instance (ITraversable OrgF)

type Org = Fix (ComposeIx [] OrgF)
deriving newtype instance Semigroup (Org ix)
deriving newtype instance Monoid (Org ix)

-- * Objects

pattern OrgObject :: StandardProperties -> Object -> OrgObjectData k 'ObjIx -> OrgF k ObjIx
pattern OrgObject props annotations datum = OrgF props annotations (P.OrgObjectF datum)
{-# COMPLETE OrgObject #-}

pattern OrgObject' :: () => (ix ~ 'ObjIx) => StandardProperties -> Object -> OrgObjectData k 'ObjIx -> OrgF k ix
pattern OrgObject' props annotations datum = OrgF props annotations (P.OrgObjectF datum)

object :: StandardProperties -> Object -> OrgObjectD -> Org ObjIx
object props annotations datum = coerce $ (: []) $ OrgObject props annotations datum

type OrgObjectD = OrgObjectData Org ObjIx
type OrgObjects = Org ObjIx

-- * Elements

pattern OrgElement :: StandardProperties -> Object -> Keywords (k ObjIx) -> OrgElementData k ElmIx -> OrgF k ElmIx
pattern OrgElement props annotations keywords datum = OrgF props annotations (P.OrgElementF keywords datum)
{-# COMPLETE OrgElement #-}

pattern OrgElement' :: () => (ix ~ 'ElmIx) => StandardProperties -> Object -> Keywords (k 'ObjIx) -> OrgElementData k 'ElmIx -> OrgF k ix
pattern OrgElement' props annotations keywords datum = OrgF props annotations (P.OrgElementF keywords datum)

element :: StandardProperties -> Object -> Keywords (Org ObjIx) -> OrgElementD -> Org ElmIx
element props annotations keywords datum = coerce $ (: []) $ OrgElement props annotations keywords datum

type OrgElementD = OrgElementData Org ElmIx
type OrgElements = Org ElmIx

-- * Sections

pattern OrgSection :: StandardProperties -> Object -> OrgSectionData k SecIx -> OrgF k SecIx
pattern OrgSection props annotations datum = OrgF props annotations (P.OrgSectionF datum)
{-# COMPLETE OrgSection #-}

pattern OrgSection' :: () => (ix ~ 'SecIx) => StandardProperties -> Object -> OrgSectionData k 'SecIx -> OrgF k ix
pattern OrgSection' props annotations datum = OrgF props annotations (P.OrgSectionF datum)

section :: StandardProperties -> Object -> OrgSectionD -> Org SecIx
section props annotations datum = coerce $ (: []) $ OrgSection props annotations datum

type OrgSectionD = OrgSectionData Org SecIx
type OrgSections = Org SecIx

type OrgDocument = OrgDocumentData Org ElmIx
