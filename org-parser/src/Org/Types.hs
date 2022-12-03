{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Org.Types where

import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Text qualified as T

-- * Document, Sections and Headings

data OrgDocument = OrgDocument
  { documentProperties :: Properties,
    documentChildren :: [OrgElement],
    documentSections :: [OrgSection]
  }
  deriving (Eq, Ord, Read, Show, Generic)

lookupProperty :: Text -> OrgDocument -> Maybe Text
lookupProperty k = M.lookup k . documentProperties

data OrgSection = OrgSection
  { sectionLevel :: Int,
    sectionProperties :: Properties,
    sectionTodo :: Maybe TodoKeyword,
    sectionIsComment :: Bool,
    sectionPriority :: Maybe Priority,
    sectionTitle :: [OrgObject],
    sectionRawTitle :: Text,
    sectionAnchor :: Id,
    sectionTags :: Tags,
    sectionPlanning :: PlanningInfo,
    sectionChildren :: [OrgElement],
    sectionSubsections :: [OrgSection]
  }
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

lookupSectionProperty :: Text -> OrgSection -> Maybe Text
lookupSectionProperty k = M.lookup k . sectionProperties

type OrgContent = ([OrgElement], [OrgSection])

documentContent :: OrgDocument -> OrgContent
documentContent doc = (documentChildren doc, documentSections doc)

mapContentM :: Monad m => (OrgContent -> m OrgContent) -> OrgDocument -> m OrgDocument
mapContentM f d = do
  (c', s') <- f (documentContent d)
  pure $ d {documentChildren = c', documentSections = s'}

mapContent :: (OrgContent -> OrgContent) -> OrgDocument -> OrgDocument
mapContent f = runIdentity . mapContentM (Identity . f)

sectionContent :: OrgSection -> OrgContent
sectionContent sec = (sectionChildren sec, sectionSubsections sec)

mapSectionContentM :: Monad m => (OrgContent -> m OrgContent) -> OrgSection -> m OrgSection
mapSectionContentM f d = do
  (c', s') <- f (sectionContent d)
  pure $ d {sectionChildren = c', sectionSubsections = s'}

mapSectionContent :: (OrgContent -> OrgContent) -> OrgSection -> OrgSection
mapSectionContent f = runIdentity . mapSectionContentM (Identity . f)

type Tag = Text

type Tags = [Tag]

-- | The states in which a todo item can be
data TodoState = Todo | Done
  deriving (Eq, Ord, Show, Read, Generic)

-- | A to-do keyword like @TODO@ or @DONE@.
data TodoKeyword = TodoKeyword
  { todoState :: TodoState,
    todoName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic)

data Priority
  = LetterPriority Char
  | NumericPriority Int
  deriving (Show, Eq, Ord, Read, Generic)

type Date = (Int, Int, Int, Maybe Text)

type Time = (Int, Int)

type TimestampMark = (Text, Int, Char)

type DateTime = (Date, Maybe Time, Maybe TimestampMark, Maybe TimestampMark)

-- | An Org timestamp, including repetition marks.
data TimestampData
  = TimestampData Bool DateTime
  | TimestampRange Bool DateTime DateTime
  deriving (Show, Eq, Ord, Read, Generic)

-- | Planning information for a subtree/headline.
data PlanningInfo = PlanningInfo
  { planningClosed :: Maybe TimestampData,
    planningDeadline :: Maybe TimestampData,
    planningScheduled :: Maybe TimestampData
  }
  deriving (Show, Eq, Ord, Read, Generic)

type Properties = Map Text Text

-- * Elements

-- | Org element. Like a Pandoc Block.
data OrgElement
  = -- | Greater block
    GreaterBlock
      { -- | Affiliated keywords
        affKws :: AffKeywords,
        -- | Greater block type
        blkType :: GreaterBlockType,
        -- | Greater block elements
        blkElements :: [OrgElement]
      }
  | -- | Drawer
    Drawer
      { -- | Drawer name
        drawerName :: Text,
        -- | Drawer elements
        drawerElements :: [OrgElement]
      }
  | -- | Plain list
    PlainList
      { -- | Affiliated keywords
        affKws :: AffKeywords,
        -- | List types
        listType :: ListType,
        -- | List items
        listItems :: [ListItem]
      }
  | -- | Export block
    ExportBlock
      Text
      -- ^ Format
      Text
      -- ^ Contents
  | -- | Example block
    ExampleBlock
      AffKeywords
      -- ^ Affiliated keywords
      (Map Text Text)
      -- ^ Switches
      [SrcLine]
      -- ^ Contents
  | -- | Source blocks
    SrcBlock
      { -- | Affiliated keywords
        affKws :: AffKeywords,
        -- | Language
        srcBlkLang :: Text,
        -- | Switches
        srcBlkSwitches :: Map Text Text,
        -- | Header arguments
        srcBlkArguments :: [(Text, Text)],
        -- | Contents
        srcBlkLines :: [SrcLine]
      }
  | VerseBlock AffKeywords [[OrgObject]]
  | HorizontalRule
  | Keyword Text Text
  | LaTeXEnvironment
      AffKeywords
      Text
      -- ^ Environment name
      Text
      -- ^ Environment contents
  | Paragraph AffKeywords [OrgObject]
  | Table AffKeywords [TableRow]
  | FootnoteDef
      Text
      -- ^ Footnote name
      [OrgElement]
      -- ^ Footnote content
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

data SrcLine
  = SrcLine Text
  | RefLine
      Id
      -- ^ Reference id (its anchor)
      Text
      -- ^ Reference name (how it appears)
      Text
      -- ^ Line contents
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

srcLineContent :: SrcLine -> Text
srcLineContent (SrcLine c) = c
srcLineContent (RefLine _ _ c) = c

srcLineMap :: (Text -> Text) -> SrcLine -> SrcLine
srcLineMap f (SrcLine c) = SrcLine (f c)
srcLineMap f (RefLine i t c) = RefLine i t (f c)

-- Keywords and AffKeywords keywords

data AffKeywordValue
  = ValueKeyword Text
  | ParsedKeyword [OrgObject] [OrgObject]
  | BackendKeyword [(Text, Text)]
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

instance Semigroup AffKeywordValue where
  (ValueKeyword t1) <> (ValueKeyword t2) = ValueKeyword (t1 <> "\n" <> t2)
  (ParsedKeyword o1 t1) <> (ParsedKeyword o2 t2) = ParsedKeyword (o1 <> o2) (t1 <> t2)
  (BackendKeyword b1) <> (BackendKeyword b2) = BackendKeyword (b1 <> b2)
  _ <> x = x

type AffKeywords = Map Text AffKeywordValue

keywordsFromList :: [(Text, AffKeywordValue)] -> AffKeywords
keywordsFromList = M.fromListWith (flip (<>))

-- Greater Blocks

data GreaterBlockType = Center | Quote | Special Text
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

-- Lists

data ListType = Ordered OrderedStyle | Descriptive | Unordered Char
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

data OrderedStyle = OrderedNum | OrderedAlpha
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

orderedStyle :: Text -> OrderedStyle
orderedStyle (T.any isDigit -> True) = OrderedNum
orderedStyle _ = OrderedAlpha

-- | One item of a list. Parameters are bullet, counter cookie, checkbox and
-- tag.
data ListItem = ListItem Bullet (Maybe Int) (Maybe Checkbox) [OrgObject] [OrgElement]
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

data Bullet = Bullet Char | Counter Text Char
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

listItemType :: ListItem -> ListType
listItemType (ListItem (Counter t _) _ _ _ _) = Ordered (orderedStyle t)
listItemType (ListItem (Bullet _) _ _ (_ : _) _) = Descriptive
listItemType (ListItem (Bullet c) _ _ _ _) = Unordered c

-- Babel call

data BabelCall = BabelCall
  { babelCallName :: Text,
    babelCallHeader1 :: Text,
    babelCallHeader2 :: Text,
    babelCallArguments :: Text
  }
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

-- Tables

data TableRow
  = StandardRow [TableCell]
  | ColumnPropsRow [Maybe ColumnAlignment]
  | RuleRow
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

type TableCell = [OrgObject]

data ColumnAlignment = AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

-- * Objects (inline elements)

-- | Objects (inline elements).
data OrgObject
  = Plain Text
  | SoftBreak
  | LineBreak
  | Italic [OrgObject]
  | Underline [OrgObject]
  | Bold [OrgObject]
  | Strikethrough [OrgObject]
  | Superscript [OrgObject]
  | Subscript [OrgObject]
  | Quoted QuoteType [OrgObject]
  | Code Text
  | Verbatim Text
  | Timestamp TimestampData
  | -- | Replacement commands (e.g. @\alpha{}@)
    Entity
      Text
      -- ^ Entity name (e.g. @"alpha"@)
  | LaTeXFragment FragmentType Text
  | ExportSnippet Text Text
  | -- | Footnote reference.
    FootnoteRef FootnoteRefData
  | Cite Citation
  | InlBabelCall BabelCall
  | Src Text Text Text
  | Link LinkTarget [OrgObject]
  | Image LinkTarget
  | -- | Inline target (e.g. @<<<foo>>>@)
    Target
      Id
      -- ^ Anchor
      Text
      -- ^ Name
  | -- | Org inline macro (e.g. @{{{poem(red,blue)}}}@)
    Macro
      Text
      -- ^ Macro name (e.g. @"poem"@)
      [Text]
      -- ^ Arguments (e.g. @["red", "blue"]@)
  | -- | Statistic cookies.
    StatisticCookie
      (Either (Int, Int) Int)
      -- ^ Either [num1/num2] or [percent%].
  deriving (Show, Eq, Ord, Read, Typeable, Generic)

data FootnoteRefData
  = FootnoteRefLabel Text
  | FootnoteRefDef (Maybe Text) [OrgObject]
  deriving (Show, Eq, Ord, Read, Typeable, Generic)

type Protocol = Text

type Id = Text

data LinkTarget
  = URILink Protocol Text
  | InternalLink Id
  | UnresolvedLink Text
  deriving (Show, Eq, Ord, Read, Typeable, Generic)

data FragmentType
  = RawFragment
  | InlMathFragment
  | DispMathFragment
  deriving (Show, Eq, Ord, Read, Typeable, Generic)

data Citation = Citation
  { citationStyle :: Text,
    citationVariant :: Text,
    citationPrefix :: [OrgObject],
    citationSuffix :: [OrgObject],
    citationReferences :: [CiteReference]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Generic)

data CiteReference = CiteReference
  { refId :: Text,
    refPrefix :: [OrgObject],
    refSuffix :: [OrgObject]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Generic)

{- ORMOLU_DISABLE -}
instance NFData OrgDocument
instance NFData AffKeywordValue
instance NFData FootnoteRefData
instance NFData OrgObject
instance NFData QuoteType
instance NFData BabelCall
instance NFData TimestampData
instance NFData FragmentType
instance NFData Citation
instance NFData CiteReference
instance NFData LinkTarget
instance NFData OrgElement
instance NFData GreaterBlockType
instance NFData ListType
instance NFData OrderedStyle
instance NFData ListItem
instance NFData Bullet
instance NFData Checkbox
instance NFData SrcLine
instance NFData TableRow
instance NFData ColumnAlignment
instance NFData OrgSection
instance NFData TodoKeyword
instance NFData TodoState
instance NFData Priority
instance NFData PlanningInfo
