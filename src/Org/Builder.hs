{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
  StandaloneDeriving, DeriveTraversable, DeriveGeneric, TypeFamilies #-}

module Org.Builder where

import Org.Types
import Data.Sequence ((|>), viewr, viewl, ViewR(..), ViewL(..))
import qualified GHC.Exts
import qualified Data.Text as T

newtype Many a = Many { unMany :: Seq a }
  deriving (Ord, Eq, Typeable, Foldable, Traversable, Functor, Show, Read)

instance One (Many a) where
  type OneItem (Many a) = a
  one = Many . one

instance IsList (Many a) where
  type Item (Many a) = a
  fromList = Many . fromList
  toList = toList . unMany

deriving instance Generic (Many a)

type OrgInlines = Many OrgInline
type OrgElements  = Many OrgElement

deriving instance Semigroup OrgElements
deriving instance Monoid OrgElements

instance Semigroup OrgInlines where
  (Many xs) <> (Many ys) =
    case (viewr xs, viewl ys) of
      (EmptyR, _) -> Many ys
      (_, EmptyL) -> Many xs
      (xs' :> x, y :< ys') -> Many (meld <> ys')
        where
          meld =
            case (x, y) of
              (Plain t1, Plain t2) -> xs' |> Plain (t1 <> t2)
              (Plain t1, LineBreak) -> xs' |> Plain (T.stripEnd t1) |> LineBreak
              (LineBreak, Plain t2) -> xs' |> LineBreak |> Plain (T.stripEnd t2)
              (Italic i1, Italic i2) -> xs' |> Italic (i1 <> i2)
              (Underline i1, Underline i2) -> xs' |> Underline (i1 <> i2)
              (Bold i1, Bold i2) -> xs' |> Bold (i1 <> i2)
              (Subscript i1, Subscript i2) -> xs' |> Subscript (i1 <> i2)
              (Superscript i1, Superscript i2) -> xs' |> Superscript (i1 <> i2)
              (Strikethrough i1, Strikethrough i2) -> xs' |> Strikethrough (i1 <> i2)
              (Code i1, Code i2) -> xs' |> Code (i1 <> i2)
              (Verbatim i1, Verbatim i2) -> xs' |> Verbatim (i1 <> i2)
              (SoftBreak, LineBreak) -> xs' |> LineBreak
              (LineBreak, SoftBreak) -> xs' |> LineBreak
              (SoftBreak, SoftBreak) -> xs' |> SoftBreak
              _                  -> xs' |> x |> y

instance Monoid OrgInlines where
  mempty = Many mempty
  mappend = (<>)

instance IsString OrgInlines where
   fromString = text . T.pack

-- * Element builders

para :: Affiliated -> OrgInlines -> OrgElements
para aff = one . Paragraph aff . toList

export :: Text -> Text -> OrgElements
export format = one . ExportBlock format

example ::
  Affiliated ->
  Maybe Int ->
  [SrcLine] ->
  OrgElements
example aff nums = one . ExampleBlock aff nums

srcBlock ::
  Affiliated ->
  Text ->
  Maybe Int ->
  Map Text Text ->
  [SrcLine] ->
  OrgElements
srcBlock aff lang nums args = one . SrcBlock aff lang nums args

list ::
  Affiliated ->
  ListType ->
  [ListItem] ->
  OrgElements
list aff kind = one . PlainList aff kind

-- * Object builders

text :: Text -> OrgInlines
text = fromList . map conv . breakByNewline
  where breakByNewline = T.groupBy sameCategory
        sameCategory x y = is_newline x == is_newline y
        conv xs | T.any is_newline xs = SoftBreak
        conv xs = Plain xs
        is_newline '\r' = True
        is_newline '\n' = True
        is_newline _    = False

plain :: Text -> OrgInlines
plain = one . Plain

italic :: OrgInlines -> OrgInlines
italic = one . Italic . toList

underline :: OrgInlines -> OrgInlines
underline = one . Underline . toList

bold :: OrgInlines -> OrgInlines
bold = one . Bold . toList

strikethrough :: OrgInlines -> OrgInlines
strikethrough = one . Strikethrough . toList

superscript :: OrgInlines -> OrgInlines
superscript = one . Superscript . toList

subscript :: OrgInlines -> OrgInlines
subscript = one . Subscript . toList

singleQuoted :: OrgInlines -> OrgInlines
singleQuoted = quoted SingleQuote

doubleQuoted :: OrgInlines -> OrgInlines
doubleQuoted = quoted DoubleQuote

quoted :: QuoteType -> OrgInlines -> OrgInlines
quoted qt = one . Quoted qt . toList

citation :: Citation -> OrgInlines
citation = one . Cite

citation' :: Text -> Text -> OrgInlines -> OrgInlines -> [CiteReference] -> OrgInlines
citation' style variant prefix suffix = one . Cite . Citation style variant (toList prefix) (toList suffix)

timestamp :: TimestampData -> OrgInlines
timestamp = one . Timestamp

-- | Plain inline code.
code :: Text -> OrgInlines
code = one . Code

-- | Inline verbatim.
verbatim :: Text -> OrgInlines
verbatim = one . Verbatim

softbreak :: OrgInlines
softbreak = one SoftBreak

linebreak :: OrgInlines
linebreak = one LineBreak

entity :: Text -> OrgInlines
entity = one . Entity

fragment :: Text -> OrgInlines
fragment = one . LaTeXFragment RawFragment

inlMath :: Text -> OrgInlines
inlMath = one . LaTeXFragment InlMathFragment

dispMath :: Text -> OrgInlines
dispMath = one . LaTeXFragment DispMathFragment

exportSnippet :: Text -> Text -> OrgInlines
exportSnippet backend = one . ExportSnippet backend

inlBabel :: Text -> Text -> Text -> Text -> OrgInlines
inlBabel name h1 h2 args = one $ InlBabelCall (BabelCall name h1 h2 args)

inlSrc :: Text -> Text -> Text -> OrgInlines
inlSrc name headers = one . Src name headers

link :: LinkTarget -> OrgInlines -> OrgInlines
link target = one . Link target . toList

uriLink :: Text -> Text -> OrgInlines -> OrgInlines
uriLink protocol tgt = one . Link (URILink protocol tgt) . toList

image :: LinkTarget -> OrgInlines
image = one . Image

horizontalRule :: OrgElements
horizontalRule = one HorizontalRule

special :: Text -> OrgInlines -> OrgInlines
special s = one . SpanSpecial s . toList
