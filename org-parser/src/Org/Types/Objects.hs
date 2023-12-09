module Org.Types.Objects where

import Data.Data (Data)
import Org.Types.StandardProperties (Pos)

-- * Objects (inline elements)

-- | Objects (inline elements).
newtype OrgObject = OrgObject {object :: OrgObjectData OrgObject}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- | Objects (inline elements).
data OrgObjectWPos = OrgObjectWPos
  { pos :: Pos
  , object :: OrgObjectData OrgObjectWPos
  }
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
      -- | Name (e.g. @alpha@)
      Text
  | LaTeXFragment FragmentType Text
  | -- | Inline export snippet (e.g. @\@\@html:\<br/\>\@\@@)
    ExportSnippet
      -- | Back-end (e.g. @html@)
      Text
      -- | Value (e.g. @\<br/\>@)
      Text
  | -- | Footnote reference.
    FootnoteRef (FootnoteRefData o)
  | Cite (Citation o)
  | InlBabelCall BabelCall
  | -- | Inline source (e.g. @src_html[:foo bar]{\<br/\>}@)
    Src
      -- | Language (e.g. @html@)
      Text
      -- | Parameters (e.g. @:foo bar@)
      Text
      -- | Value (e.g. @\<br/\>@)
      Text
  | Link LinkTarget [o]
  | -- | Inline target (e.g. @\<\<\<foo\>\>\>@)
    Target
      -- | Name
      Text
  | -- | Org inline macro (e.g. @{{{poem(red,blue)}}}@)
    Macro
      -- | Macro name (e.g. @"poem"@)
      Text
      -- | Arguments (e.g. @["red", "blue"]@)
      [Text]
  | -- | Statistic cookies.
    StatisticCookie
      -- | Either @[num1/num2]@ or @[percent%]@.
      (Either (Int, Int) Int)
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

-- | Data for a footnote reference.
data FootnoteRefData o
  = -- | Label-only footnote reference (e.g. @[fn:foo]@)
    FootnoteRefLabel
      -- | Label (e.g. @foo@)
      Text
  | -- | Inline footnote definition (e.g. @[fn:foo::bar]@)
    FootnoteRefDef
      -- | Label (if present, e.g. @foo@)
      (Maybe Text)
      -- | Content (e.g. @bar@)
      [o]
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
  , prefix :: o
  , suffix :: o
  , references :: [CiteReference o]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  deriving anyclass (NFData)

data CiteReference o = CiteReference
  { id :: Text
  , prefix :: o
  , suffix :: o
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
