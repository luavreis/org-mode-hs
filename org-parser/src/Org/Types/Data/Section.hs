{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Data.Section
  ( OrgSectionData (..)
  , TodoKeyword (..)
  , Properties
  , TodoState (..)
  , Tag
  , Priority (..)
  , PlanningInfo (..)
  ) where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Aeson qualified as Aeson
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.Traversable (ITraversable)
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Aeson (aesonOptions)
import Org.Types.Data.Timestamp (TimestampData)
import Org.Types.Ix

data OrgSectionData k (_i :: OrgIx) = OrgSectionData
  { level :: Int
  , properties :: Properties
  , todo :: Maybe TodoKeyword
  , comment :: Bool
  , priority :: Maybe Priority
  , title :: k ObjIx
  , rawTitle :: Text
  , tags :: [Tag]
  , planning :: PlanningInfo
  , children :: k ElmIx
  , subsections :: k SecIx
  }
  deriving (Typeable, Generic)

deriving instance (Show (k ObjIx), Show (k ElmIx), Show (k SecIx)) => Show (OrgSectionData k ix)
deriving instance (Read (k ObjIx), Read (k ElmIx), Read (k SecIx)) => Read (OrgSectionData k ix)
deriving instance (Eq (k ObjIx), Eq (k ElmIx), Eq (k SecIx)) => Eq (OrgSectionData k ix)
deriving instance (Ord (k ObjIx), Ord (k ElmIx), Ord (k SecIx)) => Ord (OrgSectionData k ix)
deriving instance (NFData (k ObjIx), NFData (k ElmIx), NFData (k SecIx)) => NFData (OrgSectionData k ix)

type Tag = Text

-- | The states in which a todo item can be
data TodoState = Todo | Done
  deriving (Eq, Ord, Show, Read, Typeable, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TodoState where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance Aeson.FromJSON TodoState where
  parseJSON = Aeson.genericParseJSON aesonOptions

-- | A to-do keyword like @TODO@ or @DONE@.
data TodoKeyword = TodoKeyword {state :: TodoState, name :: Text}
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TodoKeyword where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance Aeson.FromJSON TodoKeyword where
  parseJSON = Aeson.genericParseJSON aesonOptions

data Priority
  = LetterPriority Char
  | NumericPriority Int
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

-- | Planning information for a subtree/headline.
data PlanningInfo = PlanningInfo
  { closed :: Maybe TimestampData
  , deadline :: Maybe TimestampData
  , scheduled :: Maybe TimestampData
  }
  deriving (Show, Eq, Ord, Read, Typeable, Generic)
  deriving anyclass (NFData)

type Properties = Map Text Text

$(deriveGenericK ''OrgSectionData)
deriving via (Generically OrgSectionData) instance (Endofunctor (~>) OrgSectionData)
deriving via (Generically OrgSectionData) instance (IFoldable OrgSectionData)
deriving via (Generically OrgSectionData) instance (ITraversable OrgSectionData)
