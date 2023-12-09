module Org.Types.Elements where

import Data.Char (isDigit)
import Data.Data (Data)
import Data.Map qualified as M
import Data.Text qualified as T
import Org.Types.Objects (OrgObject, OrgObjectWPos, OrgTime, TimestampData)
import Org.Types.StandardProperties (Pos)

-- * Elements

-- | Org element. Like a Pandoc Block.
data OrgElement = OrgElement
  { affKeywords :: Keywords [OrgObject]
  , element :: OrgElementData [OrgObject] [OrgElement]
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type OrgElementD =  OrgElementData OrgObject OrgElement

-- | Org element. Like a Pandoc Block.
data OrgElementWPos = OrgElementWPos
  { pos :: Pos
  , affKeywords :: Keywords [OrgObjectWPos]
  , element :: OrgElementData [OrgObjectWPos] [OrgElementWPos]
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type OrgElementWPosD =  OrgElementData OrgObjectWPos OrgElementWPos

data OrgElementData o e
  = -- | Clock
    Clock
      -- | Clock timestamp
      TimestampData
      -- | Duration
      (Maybe OrgTime)
  | -- | Greater block
    GreaterBlock
      { blkType :: GreaterBlockType
      -- ^ Greater block type
      , blkElements :: e
      -- ^ Greater block elements
      }
  | -- | Drawer
    Drawer
      { drawerName :: Text
      -- ^ Drawer name
      , drawerElements :: e
      -- ^ Drawer elements
      }
  | -- | Plain list
    PlainList
      { listType :: ListType
      -- ^ List types
      , listItems :: [ListItem o e]
      -- ^ List items
      }
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
      { srcBlkLang :: Text
      -- ^ Language
      , srcBlkSwitches :: Map Text Text
      -- ^ Switches
      , srcBlkArguments :: [(Text, Text)]
      -- ^ Header arguments
      , srcBlkLines :: [SrcLine]
      -- ^ Contents
      }
  | VerseBlock [o]
  | HorizontalRule
  | Keyword
      { keywordKey :: Text
      , keywordValue :: KeywordValue o
      }
  | LaTeXEnvironment
      -- | Environment name
      Text
      -- | Environment contents
      Text
  | Paragraph o
  | Table [TableRow o]
  | FootnoteDef
      -- | Footnote name
      Text
      -- | Footnote content
      e
  | Comment
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data SrcLine
  = SrcLine Text
  | RefLine
      -- | Reference name (how it appears)
      Text
      -- | Line contents
      Text
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

srcLineContent :: SrcLine -> Text
srcLineContent (SrcLine c) = c
srcLineContent (RefLine _ c) = c

srcLinesToText :: [SrcLine] -> Text
srcLinesToText = T.unlines . map srcLineContent

srcLineMap :: (Text -> Text) -> SrcLine -> SrcLine
srcLineMap f (SrcLine c) = SrcLine (f c)
srcLineMap f (RefLine i c) = RefLine i (f c)

-- Keywords

data KeywordValue o
  = ValueKeyword Text
  | ParsedKeyword o
  | BackendKeyword [(Text, Text)]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

instance Semigroup o => Semigroup (KeywordValue o) where
  (ValueKeyword t1) <> (ValueKeyword t2) = ValueKeyword (t1 <> "\n" <> t2)
  (ParsedKeyword t1) <> (ParsedKeyword t2) = ParsedKeyword (t1 <> t2)
  (BackendKeyword b1) <> (BackendKeyword b2) = BackendKeyword (b1 <> b2)
  _ <> x = x

type Keywords o = Map Text (KeywordValue o)

lookupValueKeyword :: Text -> Keywords o -> Text
lookupValueKeyword key kws = fromMaybe mempty do
  ValueKeyword x <- M.lookup key kws
  return x

lookupParsedKeyword :: Monoid o => Text -> Keywords o -> o
lookupParsedKeyword key kws = fromMaybe mempty do
  ParsedKeyword x <- M.lookup key kws
  return x

lookupBackendKeyword :: Text -> Keywords o -> [(Text, Text)]
lookupBackendKeyword key kws = fromMaybe mempty do
  BackendKeyword x <- M.lookup key kws
  return x

keywordsFromList :: Semigroup o => [(Text, KeywordValue o)] -> Keywords o
keywordsFromList = M.fromListWith (flip (<>))

-- Greater Blocks

data GreaterBlockType = Center | Quote | Special Text
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- Lists

data ListType = Ordered OrderedStyle | Descriptive | Unordered Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data OrderedStyle = OrderedNum | OrderedAlpha
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

orderedStyle :: Text -> OrderedStyle
orderedStyle (T.any isDigit -> True) = OrderedNum
orderedStyle _ = OrderedAlpha

{- | One item of a list. Parameters are bullet, counter cookie, checkbox and
tag.
-}
data ListItem o e = ListItem Bullet (Maybe Int) (Maybe Checkbox) (Maybe o) e
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data Bullet = Bullet Char | Counter Text Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

listItemType :: ListItem o e -> ListType
listItemType (ListItem (Counter t _) _ _ _ _) = Ordered (orderedStyle t)
listItemType (ListItem Bullet {} _ _ Just {} _) = Descriptive
listItemType (ListItem (Bullet c) _ _ _ _) = Unordered c

-- Tables

data TableRow o
  = StandardRow [o]
  | ColumnPropsRow [Maybe ColumnAlignment]
  | RuleRow
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data ColumnAlignment = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)
