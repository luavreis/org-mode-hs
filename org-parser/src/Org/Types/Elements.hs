module Org.Types.Elements where

import Data.Char (isDigit)
import Data.Data (Data)
import Data.Map qualified as M
import Data.Text qualified as T
import Org.Types.Objects (OrgObject, OrgTime, TimestampData)

-- * Elements

-- | Org element. Like a Pandoc Block.
data OrgElement = OrgElement {affiliatedKeywords :: Keywords, elementData :: OrgElementData}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data OrgElementData
  = -- | Clock
    Clock
      TimestampData
      -- ^ Clock timestamp
      (Maybe OrgTime)
      -- ^ Duration
  | -- | Greater block
    GreaterBlock
      { blkType :: GreaterBlockType
      -- ^ Greater block type
      , blkElements :: [OrgElement]
      -- ^ Greater block elements
      }
  | -- | Drawer
    Drawer
      { drawerName :: Text
      -- ^ Drawer name
      , drawerElements :: [OrgElement]
      -- ^ Drawer elements
      }
  | -- | Plain list
    PlainList
      { listType :: ListType
      -- ^ List types
      , listItems :: [ListItem]
      -- ^ List items
      }
  | -- | Export block
    ExportBlock
      Text
      -- ^ Format
      Text
      -- ^ Contents
  | -- | Example block
    ExampleBlock
      (Map Text Text)
      -- ^ Switches
      [SrcLine]
      -- ^ Contents
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
  | VerseBlock [[OrgObject]]
  | HorizontalRule
  | Keyword
      { keywordKey :: Text
      , keywordValue :: KeywordValue
      }
  | LaTeXEnvironment
      Text
      -- ^ Environment name
      Text
      -- ^ Environment contents
  | Paragraph [OrgObject]
  | Table [TableRow]
  | FootnoteDef
      Text
      -- ^ Footnote name
      [OrgElement]
      -- ^ Footnote content
  | Comment
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data SrcLine
  = SrcLine Text
  | RefLine
      Text
      -- ^ Reference name (how it appears)
      Text
      -- ^ Line contents
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

data KeywordValue
  = ValueKeyword Text
  | ParsedKeyword [OrgObject]
  | BackendKeyword [(Text, Text)]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

instance Semigroup KeywordValue where
  (ValueKeyword t1) <> (ValueKeyword t2) = ValueKeyword (t1 <> "\n" <> t2)
  (ParsedKeyword t1) <> (ParsedKeyword t2) = ParsedKeyword (t1 <> t2)
  (BackendKeyword b1) <> (BackendKeyword b2) = BackendKeyword (b1 <> b2)
  _ <> x = x

type Keywords = Map Text KeywordValue

lookupValueKeyword :: Text -> Keywords -> Text
lookupValueKeyword key kws = fromMaybe mempty do
  ValueKeyword x <- M.lookup key kws
  return x

lookupParsedKeyword :: Text -> Keywords -> [OrgObject]
lookupParsedKeyword key kws = fromMaybe mempty do
  ParsedKeyword x <- M.lookup key kws
  return x

lookupBackendKeyword :: Text -> Keywords -> [(Text, Text)]
lookupBackendKeyword key kws = fromMaybe mempty do
  BackendKeyword x <- M.lookup key kws
  return x

keywordsFromList :: [(Text, KeywordValue)] -> Keywords
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
data ListItem = ListItem Bullet (Maybe Int) (Maybe Checkbox) [OrgObject] [OrgElement]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data Bullet = Bullet Char | Counter Text Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

listItemType :: ListItem -> ListType
listItemType (ListItem (Counter t _) _ _ _ _) = Ordered (orderedStyle t)
listItemType (ListItem (Bullet _) _ _ (_ : _) _) = Descriptive
listItemType (ListItem (Bullet c) _ _ _ _) = Unordered c

-- Tables

data TableRow
  = StandardRow [TableCell]
  | ColumnPropsRow [Maybe ColumnAlignment]
  | RuleRow
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type TableCell = [OrgObject]

data ColumnAlignment = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)
