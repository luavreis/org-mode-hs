{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Variants.Plain
  ( OrgF (..)
  , Org
  , OrgObjects
  , OrgElements
  , OrgSections
  , OrgDocument

    -- * Constructors and patterns
  , element

    -- * Re-exports
  , module Org.Types.Data.Document
  , module Org.Types.Data.Section
  , module Org.Types.Data.Element
  , module Org.Types.Data.Object
  , module Org.Types.Data.Timestamp
  )
where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Instances
import Data.Ix.RecursionSchemes (Fix)
import GHC.Records (HasField (..))
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Document
import Org.Types.Data.Element
import Org.Types.Data.Object
import Org.Types.Data.Section
import Org.Types.Data.Timestamp
import Org.Types.Ix

data OrgF k ix where
  OrgObjectF :: OrgObjectData k ObjIx -> OrgF k ObjIx
  OrgElementF :: Keywords (k ObjIx) -> OrgElementData k ElmIx -> OrgF k ElmIx
  OrgSectionF :: OrgSectionData k SecIx -> OrgF k SecIx

instance HasField "datum" (OrgF k ObjIx) (OrgObjectData k ObjIx) where
  getField = \case OrgObjectF x -> x

instance HasField "keywords" (OrgF k ElmIx) (Keywords (k ObjIx)) where
  getField = \case OrgElementF x _ -> x

instance HasField "datum" (OrgF k ElmIx) (OrgElementData k ElmIx) where
  getField = \case OrgElementF _ x -> x

instance HasField "datum" (OrgF k SecIx) (OrgSectionData k SecIx) where
  getField = \case OrgSectionF x -> x

$(deriveGenericK ''OrgF)
deriving via (Generically OrgF) instance (Endofunctor (~>) OrgF)

type Org = Fix (ComposeIx [] OrgF)

type OrgObjects = Org ObjIx
type OrgElements = Org ElmIx
type OrgSections = Org SecIx
type OrgDocument = OrgDocumentData Org ElmIx

element :: [(Text, KeywordValue (k ObjIx))] -> OrgElementData k ElmIx -> OrgF k ElmIx
element aff = OrgElementF (fromList aff)
