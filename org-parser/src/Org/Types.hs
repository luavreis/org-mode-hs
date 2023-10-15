{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Org.Types
  ( -- * Document
    OrgDocument (..)
  , Properties

    -- ** Helpers
  , lookupProperty

    -- * Sections
  , OrgSection (..)
  , TodoKeyword (..)
  , TodoState (..)
  , Tag
  , Priority (..)
  , PlanningInfo (..)

    -- ** Helpers
  , lookupSectionProperty

    -- * OrgContent
  , OrgContent
  , documentContent
  , mapContentM
  , mapContent
  , sectionContent
  , mapSectionContentM
  , mapSectionContent

    -- * Elements
  , OrgElement (..)
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
  , TableCell
  , ColumnAlignment (..)

    -- * Objects
  , OrgObject (..)
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
  ) where

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Map qualified as M
import Org.Types.Elements
import Org.Types.Objects

-- * Document, Sections and Headings

data OrgDocument = OrgDocument
  { properties :: Properties
  , children :: [OrgElement]
  , sections :: [OrgSection]
  }
  deriving (Eq, Ord, Read, Show, Generic)
  deriving anyclass (NFData)

lookupProperty :: Text -> OrgDocument -> Maybe Text
lookupProperty k = M.lookup k . (.properties)

data OrgSection = OrgSection
  { level :: Int
  , properties :: Properties
  , todo :: Maybe TodoKeyword
  , comment :: Bool
  , priority :: Maybe Priority
  , title :: [OrgObject]
  , rawTitle :: Text
  , tags :: [Tag]
  , planning :: PlanningInfo
  , children :: [OrgElement]
  , subsections :: [OrgSection]
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

lookupSectionProperty :: Text -> OrgSection -> Maybe Text
lookupSectionProperty k = M.lookup k . (.properties)

type OrgContent = ([OrgElement], [OrgSection])

documentContent :: OrgDocument -> OrgContent
documentContent doc = (doc.children, doc.sections)

mapContentM :: Monad m => (OrgContent -> m OrgContent) -> OrgDocument -> m OrgDocument
mapContentM f d = do
  (c', s') <- f (documentContent d)
  pure $ d {children = c', sections = s'}

mapContent :: (OrgContent -> OrgContent) -> OrgDocument -> OrgDocument
mapContent f = runIdentity . mapContentM (Identity . f)

sectionContent :: OrgSection -> OrgContent
sectionContent sec = (sec.children, sec.subsections)

mapSectionContentM :: Monad m => (OrgContent -> m OrgContent) -> OrgSection -> m OrgSection
mapSectionContentM f d = do
  (c', s') <- f (sectionContent d)
  pure $ d {children = c', subsections = s'}

mapSectionContent :: (OrgContent -> OrgContent) -> OrgSection -> OrgSection
mapSectionContent f = runIdentity . mapSectionContentM (Identity . f)

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
