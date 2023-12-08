module Org.Types.Objects where

import Data.Data (Data)
import Org.Types.StandardProperties (Pos)

-- * Objects (inline elements)

-- | Objects (inline elements).
newtype OrgObject = OrgObject {objectData :: OrgObjectData OrgObject}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- | Objects (inline elements).
data OrgObjectWPos = OrgObjectWPos {objectPos :: Pos, objectData :: OrgObjectData OrgObjectWPos}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- | Objects (inline elements).
data OrgObjectData o
  = Plain Text
  | LineBreak
  | Italic [o]
  | Underline [o]
  | Bold [o]
  | Strikethrough [o]
  | Superscript [o]
  | Subscript [o]
  | Quoted QuoteType [o]
  | Code Text
  | Verbatim Text
  | Timestamp TimestampData
  | -- | Entity (e.g. @\\alpha{}@)
    Entity
      Text
      -- ^ Name (e.g. @alpha@)
  | LaTeXFragment FragmentType Text
  | -- | Inline export snippet (e.g. @\@\@html:\<br/\>\@\@@)
    ExportSnippet
      Text
      -- ^ Back-end (e.g. @html@)
      Text
      -- ^ Value (e.g. @\<br/\>@)
  | -- | Footnote reference.
    FootnoteRef (FootnoteRefData o)
  | Cite (Citation o)
  | InlBabelCall BabelCall
  | -- | Inline source (e.g. @src_html[:foo bar]{\<br/\>}@)
    Src
      Text
      -- ^ Language (e.g. @html@)
      Text
      -- ^ Parameters (e.g. @:foo bar@)
      Text
      -- ^ Value (e.g. @\<br/\>@)
  | Link LinkTarget [o]
  | -- | Inline target (e.g. @\<\<\<foo\>\>\>@)
    Target
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
      -- ^ Either @[num1/num2]@ or @[percent%]@.
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- | Data for a footnote reference.
data FootnoteRefData o
  = -- | Label-only footnote reference (e.g. @[fn:foo]@)
    FootnoteRefLabel
      Text
      -- ^ Label (e.g. @foo@)
  | -- | Inline footnote definition (e.g. @[fn:foo::bar]@)
    FootnoteRefDef
      (Maybe Text)
      -- ^ Label (if present, e.g. @foo@)
      [o]
      -- ^ Content (e.g. @bar@)
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type Protocol = Text

{- | Link target. Note that the parser does not resolve internal links. Instead,
they should be resolved using the functions in [@org-exporters@
package](https://github.com/lucasvreis/org-mode-hs). In the near future, the
'InternalLink' constructor and 'Id' type will be removed in favor of AST
extensibility. See also the documentation for 'Target'.
-}
data LinkTarget
  = URILink Protocol Text
  | UnresolvedLink Text
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

linkTargetToText :: LinkTarget -> Text
linkTargetToText = \case
  URILink prot l -> prot <> ":" <> l
  UnresolvedLink l -> l

data FragmentType
  = RawFragment
  | InlMathFragment
  | DispMathFragment
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

data Citation o = Citation
  { style :: Text
  , variant :: Text
  , prefix :: [o]
  , suffix :: [o]
  , references :: [CiteReference o]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

data CiteReference o = CiteReference
  { id :: Text
  , prefix :: [o]
  , suffix :: [o]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

data OrgDate = OrgDate {year :: Int, month :: Int, day :: Int, weekday :: Maybe Text}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

data OrgTime = OrgTime {hour :: Int, minute :: Int}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

type TimestampMark = (Text, Int, Char)

type OrgDateTime = (OrgDate, Maybe OrgTime, Maybe TimestampMark, Maybe TimestampMark)

-- | An Org timestamp, including repetition marks.
data TimestampData
  = TimestampData Bool OrgDateTime
  | TimestampRange Bool OrgDateTime OrgDateTime
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- Babel call

data BabelCall = BabelCall
  { name :: Text
  , header1 :: Text
  , header2 :: Text
  , arguments :: Text
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

