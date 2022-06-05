{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Org.Types where

import Data.Generics (Data)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Map as M

-- * Document, Sections and Headings

data OrgDocument = OrgDocument
  { documentProperties :: Properties
  , documentKeywords   :: [(KeywordKey, KeywordValue)]
  , documentFootnotes  :: Map Text [OrgElement]
  , documentChildren   :: [OrgElement]
  , documentSections   :: [OrgSection]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

lookupProperty :: Text -> OrgDocument -> Maybe Text
lookupProperty k = M.lookup k . documentProperties

lookupKeyword :: Text -> OrgDocument -> [KeywordValue]
lookupKeyword k = map snd . filter ((k ==) . fst) . documentKeywords

documentTitle :: OrgDocument -> [OrgInline]
documentTitle doc = foldMap justParsed (lookupKeyword "title" doc)
  where
    justParsed (ParsedKeyword _ o) = o
    justParsed _ = []

data OrgSection = OrgSection
  { sectionLevel       :: Int
  , sectionProperties  :: Properties
  , sectionTodo        :: Maybe TodoKeyword
  , sectionPriority    :: Maybe Priority
  , sectionTitle       :: [OrgInline]
  , sectionTags        :: Tags
  , sectionPlanning    :: PlanningInfo
  , sectionAnchor      :: Id
  , sectionChildren    :: [OrgElement]
  , sectionSubsections :: [OrgSection]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

lookupSectionProperty :: Text -> OrgSection -> Maybe Text
lookupSectionProperty k = M.lookup k . sectionProperties

type OrgContent = ([OrgElement], [OrgSection])

documentContent :: OrgDocument -> OrgContent
documentContent doc = (documentChildren doc, documentSections doc)

mapContentM :: Monad m => (OrgContent -> m OrgContent) -> OrgDocument -> m OrgDocument
mapContentM f d = do
  (c', s') <- f (documentContent d)
  pure $ d { documentChildren = c', documentSections = s' }

mapContent :: (OrgContent -> OrgContent) -> OrgDocument -> OrgDocument
mapContent f = runIdentity . mapContentM (Identity . f)

sectionContent :: OrgSection -> OrgContent
sectionContent sec = (sectionChildren sec, sectionSubsections sec)

mapSectionContentM :: Monad m => (OrgContent -> m OrgContent) -> OrgSection -> m OrgSection
mapSectionContentM f d = do
  (c', s') <- f (sectionContent d)
  pure $ d { sectionChildren = c', sectionSubsections = s' }

mapSectionContent :: (OrgContent -> OrgContent) -> OrgSection -> OrgSection
mapSectionContent f = runIdentity . mapSectionContentM (Identity . f)

type Tag = Text
type Tags = [Tag]

-- | The states in which a todo item can be
data TodoState = Todo | Done
  deriving (Eq, Ord, Show, Data, Read, Generic)

-- | A to-do keyword like @TODO@ or @DONE@.
data TodoKeyword = TodoKeyword
  { todoState :: TodoState
  , todoName  :: Text
  }
  deriving (Show, Eq, Ord, Read, Data, Generic)

data Priority
  = LetterPriority Char
  | NumericPriority Int
  deriving (Show, Eq, Ord, Read, Data, Generic)

type Date = (Int, Int, Int, Maybe Text)

type Time = (Int, Int)

type TimestampMark = (Text, Int, Char)

type DateTime = (Date, Maybe Time, Maybe TimestampMark, Maybe TimestampMark)

-- | An Org timestamp, including repetition marks.
data TimestampData
  = TimestampData Bool DateTime
  | TimestampRange Bool DateTime DateTime
  deriving (Show, Eq, Ord, Read, Data, Generic)

-- | Planning information for a subtree/headline.
data PlanningInfo = PlanningInfo
  { planningClosed    :: Maybe TimestampData
  , planningDeadline  :: Maybe TimestampData
  , planningScheduled :: Maybe TimestampData
  }
  deriving (Show, Eq, Ord, Read, Data, Generic)

type Properties = Map Text Text


-- * Elements

-- | Org element. Like a Pandoc Block.
data OrgElement
  = GreaterBlock Affiliated GreaterBlockType [OrgElement]
  | Drawer
      Text -- ^ Drawer name
      [OrgElement] -- ^ Drawer elements
  | DynamicBlock Text (Map Text Text) [OrgElement]
  | PlainList Affiliated ListType [ListItem]
  -- Table Affiliated [OrgInline] [ColSpec] TableHead [TableBody] TableFoot
  | ExportBlock
      Text -- ^ Format
      Text -- ^ Contents
  | ExampleBlock
      Affiliated -- ^ Affiliated keywords
      (Maybe Int) -- ^ Starting line number
      [SrcLine] -- ^ Contents
  | SrcBlock
      Affiliated -- ^ Affiliated keywords
      Text -- ^ Language
      (Maybe Int) -- ^ Starting line number
      [(Text, Text)] -- ^ Header arguments
      [SrcLine] -- ^ Contents
  | VerseBlock Affiliated [[OrgInline]]
  | Clock ClockData
  | HorizontalRule
  | Keyword KeywordKey KeywordValue
  | LaTeXEnvironment Affiliated Text
  | Paragraph Affiliated [OrgInline]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data SrcLine
  = SrcLine Text
  | RefLine
      Id -- ^ Reference id (its anchor)
      Text -- ^ Reference name (how it appears)
      Text -- ^ Line contents
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

srcLineContent :: SrcLine -> Text
srcLineContent (SrcLine c) = c
srcLineContent (RefLine _ _ c) = c

srcLineMap :: (Text -> Text) -> SrcLine -> SrcLine
srcLineMap f (SrcLine c) = SrcLine (f c)
srcLineMap f (RefLine i t c) = RefLine i t (f c)

-- Keywords and affiliated keywords

type KeywordKey = Text

data KeywordValue
  = ValueKeyword Text Text
  | ParsedKeyword [OrgInline] [OrgInline]
  | BackendKeyword [(Text, Text)]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Semigroup KeywordValue where
  (ValueKeyword o1 t1) <> (ValueKeyword o2 t2) = ValueKeyword (o1 <> "\n" <> o2) (t1 <> "\n" <> t2)
  (ParsedKeyword o1 t1) <> (ParsedKeyword o2 t2) = ParsedKeyword (o1 <> o2) (t1 <> t2)
  (BackendKeyword b1) <> (BackendKeyword b2) = BackendKeyword (b1 <> b2)
  _ <> x = x

type Keywords = Map KeywordKey KeywordValue

keywordsFromList :: [(KeywordKey, KeywordValue)] -> Keywords
keywordsFromList = M.fromListWith (flip (<>))

type Affiliated = Keywords


-- Greater Blocks

data GreaterBlockType = Center | Quote | Special Text
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- Lists

data ListType = Ordered OrderedStyle | Descriptive | Unordered Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data OrderedStyle = OrderedNum | OrderedAlpha
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

orderedStyle :: Text -> OrderedStyle
orderedStyle (T.any isDigit -> True) = OrderedNum
orderedStyle _ = OrderedAlpha

-- | One item of a list. Parameters are bullet, counter cookie, checkbox and
-- tag.
data ListItem = ListItem Bullet (Maybe Int) (Maybe Checkbox) [OrgInline] [OrgElement]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Bullet = Bullet Char | Counter Text Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

listItemType :: ListItem -> ListType
listItemType (ListItem (Counter t _) _ _ _ _) = Ordered (orderedStyle t)
listItemType (ListItem (Bullet '-') _ _ (_:_) _) = Descriptive
listItemType (ListItem (Bullet c) _ _ _ _) = Unordered c


-- Clock

data ClockData
  = ClockSimple DateTime
  | ClockRange DateTime DateTime Time
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- Babel call

data BabelCall = BabelCall
  { babelCallName :: Text
  , babelCallHeader1 :: Text
  , babelCallHeader2 :: Text
  , babelCallArguments :: Text
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- * Objects (inline elements)

-- | Objects (inline elements). Derived from Pandoc's Inline.
data OrgInline -- TODO rename to OrgObject
  = Plain Text
  | SoftBreak
  | LineBreak
  | NBSpace Int
  | Italic [OrgInline]
  | Underline [OrgInline]
  | Bold [OrgInline]
  | Strikethrough [OrgInline]
  | Superscript [OrgInline]
  | Subscript [OrgInline]
  | Quoted QuoteType [OrgInline]
  | Code Text
  | Verbatim Text
  | Timestamp TimestampData
  | Entity -- ^ Replacement commands (e.g. @\alpha{}@)
      Text -- ^ Entity name (e.g. @"alpha"@)
  | LaTeXFragment FragmentType Text
  | ExportSnippet Text Text
  | FootnoteRef -- ^ Footnote reference.
      Text -- ^ Label, or autogenerated id (for unlabeled footnotes).
  | Cite Citation
  | InlBabelCall BabelCall
  | Src Text Text Text
  | Link LinkTarget [OrgInline]
  | Image LinkTarget
  | Target Id
  | Macro -- ^ Org inline macro (e.g. @{{{poem(red,blue)}}}@)
      Text -- ^ Macro name (e.g. @"poem"@)
      [Text] -- ^ Arguments (e.g. @["red", "blue"]@)
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

type Protocol = Text

type Id = Text

data LinkTarget
  = URILink Protocol Text
  | InternalLink Id
  | UnresolvedLink Text
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data FragmentType
  = RawFragment
  | InlMathFragment
  | DispMathFragment
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Citation = Citation
  { citationStyle      :: Text
  , citationVariant    :: Text
  , citationPrefix     :: [OrgInline]
  , citationSuffix     :: [OrgInline]
  , citationReferences :: [CiteReference]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data CiteReference = CiteReference
  { refId      :: Text
  , refPrefix  :: [OrgInline]
  , refSuffix  :: [OrgInline]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
