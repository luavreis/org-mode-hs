{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Data.Document
  ( OrgDocumentData (..)
  ) where

import Org.Types.Data.Section (Properties)
import Org.Types.Ix
import Generics.Kind.TH (deriveGenericK)

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
