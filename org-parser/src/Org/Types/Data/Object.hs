{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Data.Object
  ( -- * Objects
    OrgObjectData (..)

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

    -- * Quotes
  , QuoteType (..)

    -- * Babel
  , BabelCall (..)
  ) where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.Traversable (ITraversable)
import Generics.Kind.TH
import Org.Types.Data.Timestamp (TimestampData)
import Org.Types.Ix

-- | Objects (inline elements).
data OrgObjectData k (_i :: OrgIx)
  = Plain Text
  | LineBreak
  | Italic (k ObjIx)
  | Underline (k ObjIx)
  | Bold (k ObjIx)
  | Strikethrough (k ObjIx)
  | Superscript (k ObjIx)
  | Subscript (k ObjIx)
  | Quoted QuoteType (k ObjIx)
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
    FootnoteRef (FootnoteRefData (k ObjIx))
  | Cite (Citation (k ObjIx))
  | InlBabelCall BabelCall
  | -- | Inline source (e.g. @src_html[:foo bar]{\<br/\>}@)
    Src
      -- | Language (e.g. @html@)
      Text
      -- | Parameters (e.g. @:foo bar@)
      Text
      -- | Value (e.g. @\<br/\>@)
      Text
  | Link LinkTarget (k ObjIx)
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
  deriving (Typeable, Generic)

deriving instance (Show (k ObjIx)) => Show (OrgObjectData k ix)
deriving instance (Read (k ObjIx)) => Read (OrgObjectData k ix)
deriving instance (Eq (k ObjIx)) => Eq (OrgObjectData k ix)
deriving instance (Ord (k ObjIx)) => Ord (OrgObjectData k ix)
deriving instance (NFData (k ObjIx)) => NFData (OrgObjectData k ix)

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
      o
  deriving (Show, Eq, Ord, Read, Typeable, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
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
  deriving (Show, Eq, Ord, Read, Typeable, Generic)
  deriving anyclass (NFData)

linkTargetToText :: LinkTarget -> Text
linkTargetToText = \case
  URILink prot l -> prot <> ":" <> l
  UnresolvedLink l -> l

data FragmentType
  = RawFragment
  | InlMathFragment
  | DispMathFragment
  deriving (Show, Eq, Ord, Read, Typeable, Generic)
  deriving anyclass (NFData)

data Citation o = Citation
  { style :: Text
  , variant :: Text
  , prefix :: o
  , suffix :: o
  , references :: [CiteReference o]
  }
  deriving (Show, Eq, Ord, Read, Typeable, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

data CiteReference o = CiteReference
  { id :: Text
  , prefix :: o
  , suffix :: o
  }
  deriving (Show, Eq, Ord, Read, Typeable, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

-- | Babel call
data BabelCall = BabelCall
  { name :: Text
  , header1 :: Text
  , header2 :: Text
  , arguments :: Text
  }
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

$(deriveGenericK ''OrgObjectData)
deriving via (Generically OrgObjectData) instance (Endofunctor (~>) OrgObjectData)
deriving via (Generically OrgObjectData) instance (IFoldable OrgObjectData)
deriving via (Generically OrgObjectData) instance (ITraversable OrgObjectData)
