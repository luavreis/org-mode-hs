{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}


module Text.Org.Types where

import Data.Generics (Data)
import Text.Pandoc.Definition
  ( Alignment
  , ColSpec
  , ColSpan
  , RowSpan
  , TableHead
  , TableBody
  , TableFoot
  , QuoteType
  , MathType
  )


-- * Document, Sections and Headings

data OrgDocument = OrgDocument
  { documentProperties :: Properties
  , documentKeywords :: [KeywordPair]
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

-- | TODO improve
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
    -- | Greater block. Greater element.
    = GreaterBlock Affiliated GreaterBlockType [OrgElement] -- Block parameters are silenced by org-element in this case
    -- | Drawer block.
    | Drawer [OrgElement]
    -- | Verse block. Argument is a list of lines.
    | VerseBlock Affiliated [[OrgInline]]
    -- | Example block with attributes.
    | ExampleBlock Affiliated Text
    -- | Export block with attributes and language.
    | ExportBlock Affiliated Text Text
    -- | Src block. First argument is language. TODO see babel spec.
    | SrcBlock Affiliated Text (Map Text Text) Text
    -- | Dynamic block with block name, block parameters and insides.
    | DynamicBlock Affiliated Text (Map Text Text) [OrgElement]
    -- | Plain list with attributes, type and items.
    | PlainList Affiliated ListType [ListItem]
    -- | Clock.
    | Clock ClockData
    -- | "Fixed-width" lines
    | FixedWidth Affiliated [[Text]]
    -- | Horizontal rule.
    | HorizontalRule
    -- | Keyword.
    | Keyword KeywordPair
    -- | LaTeX environment.
    | LaTeXEnvironment Affiliated Text Text
    -- | Table, with attributes, caption, column alignments and widths
    -- (required), table head, table bodies, and table foot.
    | Table Affiliated [OrgInline] [ColSpec] TableHead [TableBody] TableFoot
    -- | Plain text, not a paragraph.
    | Plain [OrgInline]
    -- | Paragraph.
    | Para [OrgInline]
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- * Keywords and affiliated keywords

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


-- * Blocks

data GreaterBlockType = Center | Quote | Special Text
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- * Lists

data ListType = Ordered | Descriptive | Unordered
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | One item of a list. Parameters are bullet, counter cookie, checkbox and
-- tag.
data ListItem = ListItem Bullet (Maybe Int) (Maybe Checkbox) [OrgInline] [OrgElement]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Bullet = Bullet Char | Counter Char Char
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Checkbox = BoolBox Bool | PartialBox
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

listItemType :: ListItem -> ListType
listItemType (ListItem (Counter _ _) _ _ _ _) = Ordered
listItemType (ListItem (Bullet '-') _ _ (_:_) _) = Descriptive
listItemType _ = Unordered


-- * Clock

data ClockData = ClockData
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- * Inline elements

-- | Inline elements. Derived from Pandoc's Inline.
data OrgInline
    = Str Text            -- ^ Text (string)
    | Emph [OrgInline]         -- ^ Emphasized text (list of inlines)
    | Underline [OrgInline]    -- ^  Underlined text (list of inlines)
    | Bold [OrgInline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikethrough [OrgInline]    -- ^ Strikethrough text (list of inlines)
    | Superscript [OrgInline]  -- ^ Superscripted text (list of inlines)
    | Subscript [OrgInline]    -- ^ Subscripted text (list of inlines)
    | Timestamp TimestampData
    | Quoted QuoteType [OrgInline] -- ^ Quoted text (list of inlines) TODO
    | Cite OrgCitation         -- ^ Citation
    | Code Text           -- ^ Inline code (literal) TODO babel attributes
    | Verbatim Text
    | Src Text (Map Text Text) Text
    -- | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType Text  -- ^ TeX math (literal) TODO
    | RawInline Text Text -- ^ Raw Inline TODO
    | Link [OrgInline] Text  -- ^ Hyperlink: alt text (list of inlines), target TODO
    | Image Text          -- ^ Image:  alt text (list of inlines), target TODO
    | Note [OrgElement]          -- ^ Footnote or endnote
    | Span [OrgInline]    -- ^ Generic Inline container with attributes TODO
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data OrgCitation = OrgCitation
  { citationStyle      :: Text
  , citationVariant    :: Text
  , citationPrefix     :: [OrgInline]
  , citationSuffix     :: [OrgInline]
  , citationReferences :: [OrgReference]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

data OrgReference = OrgReference
  { refId      :: Text
  , refPrefix  :: [OrgInline]
  , refSuffix  :: [OrgInline]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)

-- | A short caption, for use in, for instance, lists of figures.
type ShortCaption = [OrgInline]

-- | The caption of a table, with an optional short caption.
data Caption = Caption (Maybe ShortCaption) [OrgElement]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | A table cell.
data Cell = Cell Alignment RowSpan ColSpan [OrgElement]
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
