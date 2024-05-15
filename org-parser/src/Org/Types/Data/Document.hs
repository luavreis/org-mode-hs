{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Data.Document
  ( OrgDocumentData (..)
  ) where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.Traversable (ITraversable)
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Section (Properties)
import Org.Types.Ix

data OrgDocumentData k _i = OrgDocumentData
  { properties :: Properties
  , children :: k ElmIx
  , sections :: k SecIx
  }
  deriving (Typeable, Generic)

deriving instance (AllOrgIx Show k) => Show (OrgDocumentData k ix)
deriving instance (AllOrgIx Read k) => Read (OrgDocumentData k ix)
deriving instance (AllOrgIx Eq k) => Eq (OrgDocumentData k ix)
deriving instance (AllOrgIx Ord k) => Ord (OrgDocumentData k ix)
deriving instance (AllOrgIx NFData k) => NFData (OrgDocumentData k ix)

$(deriveGenericK ''OrgDocumentData)
deriving via (Generically OrgDocumentData) instance (Endofunctor (~>) OrgDocumentData)
deriving via (Generically OrgDocumentData) instance (IFoldable OrgDocumentData)
deriving via (Generically OrgDocumentData) instance (ITraversable OrgDocumentData)
