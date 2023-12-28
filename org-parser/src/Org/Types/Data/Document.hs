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

deriving instance (Show (k ElmIx), Show (k SecIx)) => Show (OrgDocumentData k ix)
deriving instance (Read (k ElmIx), Read (k SecIx)) => Read (OrgDocumentData k ix)
deriving instance (Eq (k ElmIx), Eq (k SecIx)) => Eq (OrgDocumentData k ix)
deriving instance (Ord (k ElmIx), Ord (k SecIx)) => Ord (OrgDocumentData k ix)
deriving instance (NFData (k ElmIx), NFData (k SecIx)) => NFData (OrgDocumentData k ix)

$(deriveGenericK ''OrgDocumentData)
deriving via (Generically OrgDocumentData) instance (Endofunctor (~>) OrgDocumentData)
deriving via (Generically OrgDocumentData) instance (IFoldable OrgDocumentData)
deriving via (Generically OrgDocumentData) instance (ITraversable OrgDocumentData)
