{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}

module Org.Types where

import Data.Generics (Data)
import Text.Pandoc.Definition
  ( TableHead
  , TableBody
  , TableFoot
  , ColSpec
  , QuoteType
  )


-- * Document, Sections and Headings

data OrgDocument = OrgDocument
  { documentProperties :: Properties
  , documentKeywords :: [KeywordPair]
  , documentFootnotes :: Map Text [OrgElement]
  , topLevelContents :: [OrgElement]
  , documentChildren :: [OrgSection]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data OrgSection = OrgSection
  { sectionLevel :: Int
  , sectionProperties :: Properties
  , sectionTodo :: Maybe TodoKeyword
  , sectionPriority :: Maybe Priority
  , sectionTitle :: [OrgInline]
  , sectionTags :: Tags
  , sectionPlanning :: PlanningInfo
  , sectionContents :: [OrgElement]
  , sectionChildren :: [OrgSection]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

type Tag = Text
type Tags = [Tag]

-- | The states in which a todo item can be
data TodoState = Todo | Done
  deriving (Eq, Ord, Show, Data, Read)

-- | A to-do keyword like @TODO@ or @DONE@.
data TodoKeyword = TodoKeyword
  { todoState :: TodoState
  , todoName  :: Text
  }
  deriving (Show, Eq, Ord, Read, Data)

data Priority
  = LetterPriority Char
  | NumericPriority Int
  deriving (Show, Eq, Ord, Read, Data)

type Date = (Int, Int, Int, Maybe Text)

type Time = (Int, Int)

data RepeaterOrDelay
  = Repeater Text Int Char
  | Delay Text Int Char
  deriving (Show, Eq, Ord, Read, Data)

type DateTime = (Date, Maybe Time, [RepeaterOrDelay])

-- | An Org timestamp, including repetition marks.
data TimestampData
  = TimestampData Bool DateTime
  | TimestampRange Bool DateTime DateTime
  deriving (Show, Eq, Ord, Read, Data)

-- | Planning information for a subtree/headline.
data PlanningInfo = PlanningInfo
  { planningClosed :: Maybe TimestampData
  , planningDeadline :: Maybe TimestampData
  , planningScheduled :: Maybe TimestampData
  }
  deriving (Show, Eq, Ord, Read, Data)

type PropertyName = Text
type PropertyValue = Text

type Properties = [(PropertyName, PropertyValue)]


-- * Elements

-- | Org element. Like a Pandoc Block.
data OrgElement
  = GreaterBlock Affiliated GreaterBlockType [OrgElement]
  | Drawer [OrgElement]
  | DynamicBlock Affiliated Text (Map Text Text) [OrgElement]
  | FootnoteDef Text
  | PlainList Affiliated ListType [ListItem]
  | Table Affiliated [OrgInline] [ColSpec] TableHead [TableBody] TableFoot
  | ExportBlock
      Affiliated -- ^ Affiliated keywords
      Text -- ^ Format
      Text -- ^ Contents
  | ExampleBlock
      Affiliated -- ^ Affiliated keywords
      (Maybe Int) -- ^ Starting line number
      (Map Text Text) -- ^ Switches
      [SrcLine] -- ^ Contents
  | SrcBlock
      Affiliated -- ^ Affiliated keywords
      Text -- ^ Language
      (Maybe Int) -- ^ Starting line number
      (Map Text Text) -- ^ Switches
      (Map Text Text) -- ^ Header arguments
      [SrcLine] -- ^ Contents
  | VerseBlock Affiliated [[OrgInline]]
  | Clock ClockData
  | FixedWidth Affiliated Text
  | HorizontalRule
  | Keyword KeywordPair
  | LaTeXEnvironment Affiliated Text Text
  | Paragraph [OrgInline]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data SrcLine
  = SrcLine Text
  | RefLine
      Text -- ^ Reference name
      Text -- ^ Line contents
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

srcLineContent :: SrcLine -> Text
srcLineContent (SrcLine c) = c
srcLineContent (RefLine _ c) = c

srcLineMap :: (Text -> Text) -> SrcLine -> SrcLine
srcLineMap f (SrcLine c) = SrcLine (f c)
srcLineMap f (RefLine t c) = RefLine t (f c)

-- Keywords and affiliated keywords

type KeywordKey = Text

data KeywordValue
  = KeywordValue Text -- everything else
  | DualKeyword Text Text -- results
  | ParsedKeyword [OrgInline] -- title, date
  | ParsedDualKeyword [OrgInline] [OrgInline] -- caption
  | BackendKeyword Text Text -- attr_backend
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

type KeywordPair = (KeywordKey, KeywordValue)

type Affiliated = Map KeywordKey (NonEmpty KeywordValue)


-- Greater Blocks

data GreaterBlockType = Center | Quote | Special Text
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- Lists

data ListType = Ordered | Descriptive | Unordered
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | One item of a list. Parameters are bullet, counter cookie, checkbox and
-- tag.
data ListItem = ListItem Bullet (Maybe Int) (Maybe Checkbox) [OrgInline] [OrgElement]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Bullet = Bullet Char | Counter Text Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

listItemType :: ListItem -> ListType
listItemType (ListItem (Counter _ _) _ _ _ _) = Ordered
listItemType (ListItem (Bullet '-') _ _ (_:_) _) = Descriptive
listItemType _ = Unordered


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
  | Italic [OrgInline]
  | Underline [OrgInline]
  | Bold [OrgInline]
  | Strikethrough [OrgInline]
  | Superscript [OrgInline]
  | Subscript [OrgInline]
  | Timestamp TimestampData
  | Quoted QuoteType [OrgInline]
  | Code Text
  | Verbatim Text
  | SoftBreak
  | Entity -- ^ Replacement commands (e.g. @\alpha{}@)
      Text -- ^ Entity name (e.g. @"alpha"@)
  | LaTeXFragment FragmentType Text
  | ExportSnippet Text Text
  | FootnoteRef Footnote
  | Cite Citation
  | InlBabelCall BabelCall
  | Src Text Text Text
  | LineBreak
  | Link LinkTarget [OrgInline]
  | Image LinkTarget
  | Macro -- ^ Org inline macro (e.g. @{{{poem(red,blue)}}}@)
      Text -- ^ Macro name (e.g. @"poem"@)
      [Text] -- ^ Arguments (e.g. @["red", "blue"]@)
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

type Protocol = Text

type Id = Text

data LinkTarget
  = URILink Protocol Text
  | InternalLink InternalLinkType Id
  | UnresolvedLink Text
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data InternalLinkType
  = Coderef
  | Anchor
  | Radio
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data FragmentType
  = RawFragment
  | InlMathFragment
  | DispMathFragment
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data Footnote
  = FootnoteLabeled Text
  | FootnoteUnlabeled [OrgInline]
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
