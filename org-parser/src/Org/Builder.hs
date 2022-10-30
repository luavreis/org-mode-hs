{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Org.Builder where

import Data.Sequence (ViewL (..), ViewR (..), viewl, viewr, (|>))
import Data.Text qualified as T
import GHC.Exts qualified
import Org.Types

newtype Many a = Many {unMany :: Seq a}
  deriving (Ord, Eq, Typeable, Foldable, Traversable, Functor, Show, Read)

instance One (Many a) where
  type OneItem (Many a) = a
  one = Many . one

instance IsList (Many a) where
  type Item (Many a) = a
  fromList = Many . fromList
  toList = toList . unMany

deriving instance Generic (Many a)

type OrgObjects = Many OrgObject

type OrgElements = Many OrgElement

deriving instance Semigroup OrgElements

deriving instance Monoid OrgElements

instance Semigroup OrgObjects where
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
              _ -> xs' |> x |> y

instance Monoid OrgObjects where
  mempty = Many mempty
  mappend = (<>)

instance IsString OrgObjects where
  fromString = text . T.pack

-- * Element builders

para :: Affiliated -> OrgObjects -> OrgElements
para aff = one . Paragraph aff . toList

export :: Text -> Text -> OrgElements
export format = one . ExportBlock format

example ::
  Affiliated ->
  Map Text Text ->
  [SrcLine] ->
  OrgElements
example aff sw = one . ExampleBlock aff sw

srcBlock ::
  Affiliated ->
  Text ->
  Map Text Text ->
  [(Text, Text)] ->
  [SrcLine] ->
  OrgElements
srcBlock aff lang sw args = one . SrcBlock aff lang sw args

greaterBlock ::
  Affiliated ->
  GreaterBlockType ->
  OrgElements ->
  OrgElements
greaterBlock aff btype = one . GreaterBlock aff btype . toList

drawer ::
  Text ->
  OrgElements ->
  OrgElements
drawer name = one . Drawer name . toList

latexEnvironment ::
  Affiliated ->
  Text ->
  Text ->
  OrgElements
latexEnvironment aff name = one . LaTeXEnvironment aff name

list ::
  Affiliated ->
  ListType ->
  [ListItem] ->
  OrgElements
list aff kind = one . PlainList aff kind

orderedList ::
  Affiliated ->
  OrderedStyle ->
  Char ->
  [OrgElements] ->
  OrgElements
orderedList aff style separator =
  one
    . PlainList aff (Ordered style)
    . zipWith (\b -> ListItem b Nothing Nothing [] . toList) bullets
  where
    bullets = case style of
      OrderedNum -> [Counter (show i) separator | i :: Int <- [1 ..]]
      OrderedAlpha -> [Counter (one a) separator | a <- ['a' ..]]

descriptiveList ::
  Affiliated ->
  [(OrgObjects, OrgElements)] ->
  OrgElements
descriptiveList aff =
  one
    . PlainList aff Descriptive
    . map (\(tag, els) -> ListItem (Bullet '-') Nothing Nothing (toList tag) (toList els))

parsedKeyword ::
  OrgObjects ->
  OrgObjects ->
  KeywordValue
parsedKeyword i = ParsedKeyword (toList i) . toList

parsedKeyword' ::
  OrgObjects ->
  KeywordValue
parsedKeyword' = parsedKeyword mempty

valueKeyword ::
  Text ->
  Text ->
  KeywordValue
valueKeyword = ValueKeyword

valueKeyword' ::
  Text ->
  KeywordValue
valueKeyword' = valueKeyword ""

attrKeyword ::
  [(Text, Text)] ->
  KeywordValue
attrKeyword = BackendKeyword

keyword ::
  KeywordKey ->
  KeywordValue ->
  OrgElements
keyword key = one . Keyword key

-- * Object builders

text :: Text -> OrgObjects
text = fromList . map conv . breakByNewline
  where
    breakByNewline = T.groupBy sameCategory
    sameCategory x y = is_newline x == is_newline y
    conv xs | T.any is_newline xs = SoftBreak
    conv xs = Plain xs
    is_newline '\r' = True
    is_newline '\n' = True
    is_newline _ = False

plain :: Text -> OrgObjects
plain = one . Plain

italic :: OrgObjects -> OrgObjects
italic = one . Italic . toList

underline :: OrgObjects -> OrgObjects
underline = one . Underline . toList

bold :: OrgObjects -> OrgObjects
bold = one . Bold . toList

strikethrough :: OrgObjects -> OrgObjects
strikethrough = one . Strikethrough . toList

superscript :: OrgObjects -> OrgObjects
superscript = one . Superscript . toList

subscript :: OrgObjects -> OrgObjects
subscript = one . Subscript . toList

singleQuoted :: OrgObjects -> OrgObjects
singleQuoted = quoted SingleQuote

doubleQuoted :: OrgObjects -> OrgObjects
doubleQuoted = quoted DoubleQuote

quoted :: QuoteType -> OrgObjects -> OrgObjects
quoted qt = one . Quoted qt . toList

citation :: Citation -> OrgObjects
citation = one . Cite

citation' :: Text -> Text -> OrgObjects -> OrgObjects -> [CiteReference] -> OrgObjects
citation' style variant prefix suffix = one . Cite . Citation style variant (toList prefix) (toList suffix)

timestamp :: TimestampData -> OrgObjects
timestamp = one . Timestamp

-- | Plain inline code.
code :: Text -> OrgObjects
code = one . Code

-- | Inline verbatim.
verbatim :: Text -> OrgObjects
verbatim = one . Verbatim

softbreak :: OrgObjects
softbreak = one SoftBreak

linebreak :: OrgObjects
linebreak = one LineBreak

entity :: Text -> OrgObjects
entity = one . Entity

fragment :: Text -> OrgObjects
fragment = one . LaTeXFragment RawFragment

inlMath :: Text -> OrgObjects
inlMath = one . LaTeXFragment InlMathFragment

dispMath :: Text -> OrgObjects
dispMath = one . LaTeXFragment DispMathFragment

exportSnippet :: Text -> Text -> OrgObjects
exportSnippet backend = one . ExportSnippet backend

inlBabel :: Text -> Text -> Text -> Text -> OrgObjects
inlBabel name h1 h2 args = one $ InlBabelCall (BabelCall name h1 h2 args)

inlSrc :: Text -> Text -> Text -> OrgObjects
inlSrc name headers = one . Src name headers

link :: LinkTarget -> OrgObjects -> OrgObjects
link tgt = one . Link tgt . toList

uriLink :: Text -> Text -> OrgObjects -> OrgObjects
uriLink protocol tgt = one . Link (URILink protocol tgt) . toList

target :: Id -> Text -> OrgObjects
target a = one . Target a

footnoteLabel :: Text -> OrgObjects
footnoteLabel = one . FootnoteRef . FootnoteRefLabel

footnoteInlDef :: Maybe Text -> OrgObjects -> OrgObjects
footnoteInlDef l = one . FootnoteRef . FootnoteRefDef l . toList

footnoteDef :: Text -> OrgElements -> OrgElements
footnoteDef l = one . FootnoteDef l . toList

statisticCookie :: Either (Int, Int) Int -> OrgObjects
statisticCookie = one . StatisticCookie

horizontalRule :: OrgElements
horizontalRule = one HorizontalRule

table :: Affiliated -> [TableRow] -> OrgElements
table aff = one . Table aff

standardRow :: [OrgObjects] -> TableRow
standardRow = StandardRow . map toList
