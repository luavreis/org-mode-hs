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
  toList = toList . (.unMany)

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
              (LineBreak, Plain t2) -> xs' |> LineBreak |> Plain (T.stripStart t2)
              (Italic i1, Italic i2) -> xs' |> Italic (i1 <> i2)
              (Underline i1, Underline i2) -> xs' |> Underline (i1 <> i2)
              (Bold i1, Bold i2) -> xs' |> Bold (i1 <> i2)
              (Subscript i1, Subscript i2) -> xs' |> Subscript (i1 <> i2)
              (Superscript i1, Superscript i2) -> xs' |> Superscript (i1 <> i2)
              (Strikethrough i1, Strikethrough i2) -> xs' |> Strikethrough (i1 <> i2)
              (Code i1, Code i2) -> xs' |> Code (i1 <> i2)
              (Verbatim i1, Verbatim i2) -> xs' |> Verbatim (i1 <> i2)
              _ -> xs' |> x |> y

instance Monoid OrgObjects where
  mempty = Many mempty
  mappend = (<>)

instance IsString OrgObjects where
  fromString = object . plain . T.pack

instance IsString OrgElements where
  fromString = element . para . object . plain . T.pack

-- * Element builders

element :: OrgElementData -> OrgElements
element = one . OrgElement mempty

element' :: [(Text, KeywordValue)] -> OrgElementData -> OrgElements
element' aff = one . OrgElement (fromList aff)

para :: OrgObjects -> OrgElementData
para = Paragraph . toList

export :: Text -> Text -> OrgElementData
export = ExportBlock

example ::
  Map Text Text ->
  [SrcLine] ->
  OrgElementData
example = ExampleBlock

srcBlock ::
  Text ->
  Map Text Text ->
  [(Text, Text)] ->
  [SrcLine] ->
  OrgElementData
srcBlock = SrcBlock

greaterBlock ::
  GreaterBlockType ->
  OrgElements ->
  OrgElementData
greaterBlock btype = GreaterBlock btype . toList

drawer ::
  Text ->
  OrgElements ->
  OrgElementData
drawer name = Drawer name . toList

latexEnvironment ::
  Text ->
  Text ->
  OrgElementData
latexEnvironment = LaTeXEnvironment

listItemUnord :: Char -> OrgElements -> ListItem
listItemUnord s = ListItem (Bullet s) Nothing Nothing [] . toList

list ::
  ListType ->
  [ListItem] ->
  OrgElementData
list = PlainList

orderedList ::
  OrderedStyle ->
  Char ->
  [OrgElements] ->
  OrgElementData
orderedList style separator =
  PlainList (Ordered style)
    . zipWith (\b -> ListItem b Nothing Nothing [] . toList) bullets
  where
    bullets = case style of
      OrderedNum -> [Counter (show i) separator | i :: Int <- [1 ..]]
      OrderedAlpha -> [Counter (one a) separator | a <- ['a' ..]]

descriptiveList ::
  [(OrgObjects, OrgElements)] ->
  OrgElementData
descriptiveList =
  PlainList Descriptive
    . map (\(tag, els) -> ListItem (Bullet '-') Nothing Nothing (toList tag) (toList els))

parsedKeyword ::
  OrgObjects ->
  KeywordValue
parsedKeyword = ParsedKeyword . toList

valueKeyword ::
  Text ->
  KeywordValue
valueKeyword = ValueKeyword

attrKeyword ::
  [(Text, Text)] ->
  KeywordValue
attrKeyword = BackendKeyword

keyword ::
  Text ->
  KeywordValue ->
  OrgElementData
keyword = Keyword

clock :: TimestampData -> Maybe OrgTime -> OrgElementData
clock = Clock

footnoteDef :: Text -> OrgElements -> OrgElementData
footnoteDef l = FootnoteDef l . toList

horizontalRule :: OrgElementData
horizontalRule = HorizontalRule

table :: [TableRow] -> OrgElementData
table = Table

standardRow :: [OrgObjects] -> TableRow
standardRow = StandardRow . map toList

-- * Object builders

object :: OrgObjectData -> OrgObjects
object = one . OrgObject mempty

plain :: Text -> OrgObjectData
plain = Plain

italic :: OrgObjects -> OrgObjectData
italic = Italic . toList

underline :: OrgObjects -> OrgObjectData
underline = Underline . toList

bold :: OrgObjects -> OrgObjectData
bold = Bold . toList

strikethrough :: OrgObjects -> OrgObjectData
strikethrough = Strikethrough . toList

superscript :: OrgObjects -> OrgObjectData
superscript = Superscript . toList

subscript :: OrgObjects -> OrgObjectData
subscript = Subscript . toList

singleQuoted :: OrgObjects -> OrgObjectData
singleQuoted = quoted SingleQuote

doubleQuoted :: OrgObjects -> OrgObjectData
doubleQuoted = quoted DoubleQuote

quoted :: QuoteType -> OrgObjects -> OrgObjectData
quoted qt = Quoted qt . toList

citation :: Citation -> OrgObjectData
citation = Cite

citation' :: Text -> Text -> OrgObjects -> OrgObjects -> [CiteReference] -> OrgObjectData
citation' style variant prefix suffix = Cite . Citation style variant (toList prefix) (toList suffix)

timestamp :: TimestampData -> OrgObjectData
timestamp = Timestamp

-- | Plain inline code.
code :: Text -> OrgObjectData
code = Code

-- | Inline verbatim.
verbatim :: Text -> OrgObjectData
verbatim = Verbatim

linebreak :: OrgObjectData
linebreak = LineBreak

entity :: Text -> OrgObjectData
entity = Entity

fragment :: Text -> OrgObjectData
fragment = LaTeXFragment RawFragment

inlMath :: Text -> OrgObjectData
inlMath = LaTeXFragment InlMathFragment

dispMath :: Text -> OrgObjectData
dispMath = LaTeXFragment DispMathFragment

exportSnippet :: Text -> Text -> OrgObjectData
exportSnippet = ExportSnippet

inlBabel :: Text -> Text -> Text -> Text -> OrgObjectData
inlBabel name h1 h2 args = InlBabelCall (BabelCall name h1 h2 args)

macro :: Text -> [Text] -> OrgObjectData
macro = Macro

inlSrc :: Text -> Text -> Text -> OrgObjectData
inlSrc = Src

link :: LinkTarget -> OrgObjects -> OrgObjectData
link tgt = Link tgt . toList

uriLink :: Text -> Text -> OrgObjects -> OrgObjectData
uriLink protocol tgt = Link (URILink protocol tgt) . toList

target :: Text -> OrgObjectData
target = Target

footnoteLabel :: Text -> OrgObjectData
footnoteLabel = FootnoteRef . FootnoteRefLabel

footnoteInlDef :: Maybe Text -> OrgObjects -> OrgObjectData
footnoteInlDef l = FootnoteRef . FootnoteRefDef l . toList

statisticCookie :: Either (Int, Int) Int -> OrgObjectData
statisticCookie = StatisticCookie
