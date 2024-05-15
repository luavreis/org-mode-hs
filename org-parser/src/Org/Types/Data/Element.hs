{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Data.Element
  ( OrgElementData (..)

    -- ** Greater blocks
  , GreaterBlockType (..)

    -- ** Source blocks
  , SrcLine
  , srcLinesToText

    -- ** Lists
  , ListType (..)
  , OrderedStyle (..)
  , orderedStyle
  , ListItem (..)
  , Bullet (..)
  , Checkbox (..)
  , listItemType

    -- * Constructors
  , listItemUnord
  , orderedList
  , descriptiveList

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
  ) where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Char (isDigit)
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.Traversable (ITraversable)
import Data.Map qualified as M
import Data.Text qualified as T
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Timestamp (OrgTime, TimestampData)
import Org.Types.Ix

data OrgElementData k (_i :: OrgIx)
  = -- | Clock
    Clock
      -- | Clock timestamp
      TimestampData
      -- | Duration
      (Maybe OrgTime)
  | -- | Greater block
    GreaterBlock
      -- | Greater block type
      GreaterBlockType
      -- | Greater block elements
      (k ElmIx)
  | -- | Drawer
    Drawer
      -- | Drawer name
      Text
      -- | Drawer elements
      (k ElmIx)
  | -- | Plain list
    PlainList
      -- | List types
      ListType
      -- | List items
      [ListItem k _i]
  | -- | Export block
    ExportBlock
      -- | Format
      Text
      -- | Contents
      Text
  | -- | Example block
    ExampleBlock
      -- | Switches
      (Map Text Text)
      -- | Contents
      [SrcLine]
  | -- | Source blocks
    SrcBlock
      { language :: Text
      -- ^ Language
      , switches :: Map Text Text
      -- ^ Switches
      , header :: [(Text, Text)]
      -- ^ Header arguments
      , srcLines :: [SrcLine]
      -- ^ Contents
      }
  | VerseBlock (k ObjIx)
  | HorizontalRule
  | Keyword
      Text
      (KeywordValue (k ObjIx))
  | LaTeXEnvironment
      -- | Environment name
      Text
      -- | Environment contents
      Text
  | Paragraph (k ObjIx)
  | Table [TableRow (k ObjIx)]
  | FootnoteDef
      -- | Footnote name
      Text
      -- | Footnote content
      (k ElmIx)
  | Comment
  deriving (Typeable, Generic)

deriving instance (AllOrgIx Show k) => Show (OrgElementData k ix)
deriving instance (AllOrgIx Read k) => Read (OrgElementData k ix)
deriving instance (AllOrgIx Eq k) => Eq (OrgElementData k ix)
deriving instance (AllOrgIx Ord k) => Ord (OrgElementData k ix)
deriving instance (AllOrgIx NFData k) => NFData (OrgElementData k ix)

type SrcLine = Text

srcLinesToText :: [SrcLine] -> Text
srcLinesToText = T.unlines

-- Keywords

data KeywordValue o
  = ValueKeyword Text
  | ParsedKeyword o
  | BackendKeyword [(Text, Text)]
  deriving (Eq, Ord, Read, Show, Typeable, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

instance (Semigroup o) => Semigroup (KeywordValue o) where
  (ValueKeyword t1) <> (ValueKeyword t2) = ValueKeyword (t1 <> "\n" <> t2)
  (ParsedKeyword t1) <> (ParsedKeyword t2) = ParsedKeyword (t1 <> t2)
  (BackendKeyword b1) <> (BackendKeyword b2) = BackendKeyword (b1 <> b2)
  _ <> x = x

type Keywords o = Map Text (KeywordValue o)

lookupValueKeyword :: Text -> Keywords o -> Text
lookupValueKeyword key kws = fromMaybe mempty do
  ValueKeyword x <- M.lookup key kws
  return x

lookupParsedKeyword :: (Monoid o) => Text -> Keywords o -> o
lookupParsedKeyword key kws = fromMaybe mempty do
  ParsedKeyword x <- M.lookup key kws
  return x

lookupBackendKeyword :: Text -> Keywords o -> [(Text, Text)]
lookupBackendKeyword key kws = fromMaybe mempty do
  BackendKeyword x <- M.lookup key kws
  return x

keywordsFromList :: (Semigroup o) => [(Text, KeywordValue o)] -> Keywords o
keywordsFromList = M.fromListWith (flip (<>))

-- Greater Blocks

data GreaterBlockType = Center | Quote | Special Text
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

-- Lists

data ListType = Ordered OrderedStyle | Descriptive | Unordered Char
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

data OrderedStyle = OrderedNum | OrderedAlpha
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

orderedStyle :: Text -> OrderedStyle
orderedStyle (T.any isDigit -> True) = OrderedNum
orderedStyle _ = OrderedAlpha

{- | One item of a list. Parameters are bullet, counter cookie, checkbox and
tag.
-}
data ListItem k (i :: OrgIx) = ListItem
  { bullet :: Bullet
  , counter :: Maybe Int
  , checkbox :: Maybe Checkbox
  , tag :: Maybe (k ObjIx)
  , content :: k ElmIx
  }
  deriving (Typeable, Generic)

deriving instance (AllOrgIx Show k) => Show (ListItem k ix)
deriving instance (AllOrgIx Read k) => Read (ListItem k ix)
deriving instance (AllOrgIx Eq k) => Eq (ListItem k ix)
deriving instance (AllOrgIx Ord k) => Ord (ListItem k ix)
deriving instance (AllOrgIx NFData k) => NFData (ListItem k ix)

data Bullet = Bullet Char | Counter Text Char
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

listItemType :: ListItem k i -> ListType
listItemType (ListItem (Counter t _) _ _ _ _) = Ordered (orderedStyle t)
listItemType (ListItem Bullet {} _ _ Just {} _) = Descriptive
listItemType (ListItem (Bullet c) _ _ _ _) = Unordered c

listItemUnord :: Char -> k ElmIx -> ListItem k ix
listItemUnord s = ListItem (Bullet s) Nothing Nothing Nothing

orderedList ::
  OrderedStyle ->
  Char ->
  [k ElmIx] ->
  OrgElementData k ix
orderedList style separator =
  PlainList (Ordered style)
    . zipWith (\b -> ListItem b Nothing Nothing Nothing) bullets
  where
    bullets = case style of
      OrderedNum -> [Counter (show i) separator | i :: Int <- [1 ..]]
      OrderedAlpha -> [Counter (one a) separator | a <- ['a' ..]]

descriptiveList :: [(k ObjIx, k ElmIx)] -> OrgElementData k ix
descriptiveList =
  PlainList Descriptive
    . map (\(tag, els) -> ListItem (Bullet '-') Nothing Nothing (Just tag) els)

-- Tables

data TableRow o
  = StandardRow [o]
  | ColumnPropsRow [Maybe ColumnAlignment]
  | RuleRow
  deriving (Eq, Ord, Read, Show, Typeable, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

data ColumnAlignment = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

$(deriveGenericK ''ListItem)
deriving via (Generically ListItem) instance (Endofunctor (~>) ListItem)
deriving via (Generically ListItem) instance (IFoldable ListItem)
deriving via (Generically ListItem) instance (ITraversable ListItem)

$(deriveGenericK ''OrgElementData)
deriving via (Generically OrgElementData) instance (Endofunctor (~>) OrgElementData)
deriving via (Generically OrgElementData) instance (IFoldable OrgElementData)
deriving via (Generically OrgElementData) instance (ITraversable OrgElementData)
