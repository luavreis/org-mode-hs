{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Org.Types
  ( -- * Document
    OrgDocumentData (..)
  , Properties

    -- * Sections
  , OrgSection
  , OrgSectionWPos
  , OrgSectionData (..)
  , TodoKeyword (..)
  , TodoState (..)
  , Tag
  , Priority (..)
  , PlanningInfo (..)

    -- * Elements
  , OrgElement (..)
  , OrgElementD
  , OrgElementWPos (..)
  , OrgElementWPosD
  , OrgElementData (..)

    -- ** Greater blocks
  , GreaterBlockType (..)

    -- ** Source blocks
  , SrcLine (..)
  , srcLineContent
  , srcLinesToText
  , srcLineMap

    -- ** Lists
  , ListType (..)
  , OrderedStyle (..)
  , orderedStyle
  , ListItem (..)
  , Bullet (..)
  , Checkbox (..)
  , listItemType

    -- ** Keywords
  , Keywords
  , KeywordValue (..)
  , lookupValueKeyword
  , lookupParsedKeyword
  , lookupBackendKeyword
  , keywordsFromList

    -- ** Tables
  , TableRow (..)
  , ColumnAlignment (..)

    -- * Objects
  , OrgObject (..)
  , OrgObjectWPos (..)
  , OrgObjectData (..)

    -- ** Links
  , LinkTarget (..)
  , Protocol
  , linkTargetToText

    -- ** LaTeX fragments
  , FragmentType (..)

    -- ** Citations
  , Citation (..)
  , CiteReference (..)

    -- ** Footnote references
  , FootnoteRefData (..)

    -- ** Timestamps
  , TimestampData (..)
  , OrgDateTime
  , TimestampMark
  , OrgDate (..)
  , OrgTime (..)

    -- * Quotes
  , QuoteType (..)

    -- * Babel
  , BabelCall (..)
  , module Org.Types.StandardProperties
  ) where

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Char (toLower)
import Data.Data (Data)
import Org.Types.Elements
import Org.Types.Objects
import Org.Types.StandardProperties

-- * Document, Sections and Headings

newtype OrgDocument = OrgDocument (OrgDocumentData OrgElement OrgSection)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

newtype OrgDocumentWPos = OrgDocumentWPos (OrgDocumentData OrgElementWPos OrgSectionWPos)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data OrgDocumentData e s = OrgDocumentData
  { properties :: Properties
  , children :: [e]
  , sections :: [s]
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

newtype OrgSection = OrgSection (OrgSectionData OrgObject OrgElement OrgSection)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

newtype OrgSectionWPos = OrgSectionWPos (OrgSectionData OrgObjectWPos OrgElementWPos OrgSectionWPos)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data OrgSectionData o e s = OrgSectionData
  { level :: Int
  , properties :: Properties
  , todo :: Maybe TodoKeyword
  , comment :: Bool
  , priority :: Maybe Priority
  , title :: [o]
  , rawTitle :: Text
  , tags :: [Tag]
  , planning :: PlanningInfo
  , children :: [e]
  , subsections :: [s]
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type Tag = Text

-- | The states in which a todo item can be
data TodoState = Todo | Done
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

instance ToJSON TodoState where
  toJSON Todo = "todo"
  toJSON Done = "done"
  toEncoding Todo = text "todo"
  toEncoding Done = text "done"

instance FromJSON TodoState where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = map toLower
        }

-- | A to-do keyword like @TODO@ or @DONE@.
data TodoKeyword = TodoKeyword
  { todoState :: TodoState
  , todoName :: Text
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

instance ToJSON TodoKeyword where
  toJSON (TodoKeyword s n) = object ["state" .= s, "name" .= n]
  toEncoding (TodoKeyword s n) = pairs ("state" .= s <> "name" .= n)

instance FromJSON TodoKeyword where
  parseJSON = withObject "Todo Keyword" $ \v ->
    TodoKeyword <$> v .: "state" <*> v .: "name"

data Priority
  = LetterPriority Char
  | NumericPriority Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- | Planning information for a subtree/headline.
data PlanningInfo = PlanningInfo
  { closed :: Maybe TimestampData
  , deadline :: Maybe TimestampData
  , scheduled :: Maybe TimestampData
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

type Properties = Map Text Text
