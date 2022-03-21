{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
  StandaloneDeriving, DeriveTraversable, DeriveGeneric, TypeFamilies #-}

module Text.Org.Builder where

import Text.Org.Types
import qualified GHC.Exts
import Data.Sequence ((|>), viewr, viewl, ViewR(..), ViewL(..))
import qualified Data.Text as T
import Text.Pandoc.Builder
  ( QuoteType (..)
  , MathType (..)
  )

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
              (Plain t1, Plain t2)   -> xs' |> Plain (t1 <> t2)
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

-- Inline list builders

-- | Convert a 'Text' to 'OrgInlines', treating interword spaces as
-- 'Space's or 'SoftBreak's. If you want a 'Str' with literal spaces,
-- use 'str'.
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

cite :: OrgCitation -> OrgInlines
cite = one . Cite

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

-- | Inline math
math :: Text -> OrgInlines
math = one . Math InlineMath

-- | Display math
displayMath :: Text -> OrgInlines
displayMath = one . Math DisplayMath

note :: OrgElements -> OrgInlines
note = one . Note . toList

para :: OrgInlines -> OrgElements
para = one . Paragraph . toList


-- lineBlock :: [OrgInlines] -> OrgElements
-- lineBlock = one . LineBlock . map toList

-- | A code block with attributes.
-- codeBlockWith :: Attr -> Text -> OrgElements
-- codeBlockWith attrs = one . CodeBlock attrs

-- | A plain code block.
-- codeBlock :: Text -> OrgElements
-- codeBlock = codeBlockWith nullAttr

-- rawBlock :: Text -> Text -> OrgElements
-- rawBlock format = one . RawBlock (Format format)

-- blockQuote :: OrgElements -> OrgElements
-- blockQuote = one . BlockQuote . toList

-- | Ordered list with attributes.
-- orderedListWith :: ListAttributes -> [OrgElements] -> OrgElements
-- orderedListWith attrs = one . OrderedList attrs .  map toList

-- | Ordered list with default attributes.
-- orderedList :: [OrgElements] -> OrgElements
-- orderedList = orderedListWith (1, DefaultStyle, DefaultDelim)

-- bulletList :: [OrgElements] -> OrgElements
-- bulletList = one . BulletList . map toList

-- definitionList :: [(OrgInlines, [OrgElements])] -> OrgElements
-- definitionList = one . DefinitionList .  map (toList *** map toList)

-- header :: Int  -- ^ Level
--        -> OrgInlines
--        -> OrgElements
-- header = headerWith nullAttr

-- headerWith :: Attr -> Int -> OrgInlines -> OrgElements
-- headerWith attr level = one . Header level attr . toList

horizontalRule :: OrgElements
horizontalRule = one HorizontalRule

-- cellWith :: Attr
--          -> Alignment
--          -> RowSpan
--          -> ColSpan
--          -> OrgElements
--          -> Cell
-- cellWith at a r c = Cell at a r c . toList

-- cell :: Alignment
--      -> RowSpan
--      -> ColSpan
--      -> OrgElements
--      -> Cell
-- cell = cellWith nullAttr

-- | A 1×1 cell with default alignment.
-- simpleCell :: OrgElements -> Cell
-- simpleCell = cell AlignDefault 1 1

-- | A 1×1 empty cell.
-- emptyCell :: Cell
-- emptyCell = simpleCell mempty

-- | Table builder. Performs normalization with 'normalizeTableHead',
-- 'normalizeTableBody', and 'normalizeTableFoot'. The number of table
-- columns is given by the length of @['ColSpec']@.
-- table :: Caption
--       -> [ColSpec]
--       -> TableHead
--       -> [TableBody]
--       -> TableFoot
--       -> OrgElements
-- table = tableWith nullAttr

-- tableWith :: Attr
--           -> Caption
--           -> [ColSpec]
--           -> TableHead
--           -> [TableBody]
--           -> TableFoot
--           -> OrgElements
-- tableWith attr capt specs th tbs tf
--   = one $ Table attr capt specs th' tbs' tf'
--   where
--     twidth = length specs
--     th'  = normalizeTableHead twidth th
--     tbs' = map (normalizeTableBody twidth) tbs
--     tf'  = normalizeTableFoot twidth tf

-- | A simple table without a caption.
-- simpleTable :: [OrgElements]   -- ^ Headers
--             -> [[OrgElements]] -- ^ Rows
--             -> OrgElements
-- simpleTable headers rows =
--   table emptyCaption (replicate numcols defaults) th [tb] tf
--   where defaults = (AlignDefault, ColWidthDefault)
--         numcols  = maximum (map length (headers:rows))
--         toRow = Row nullAttr . map simpleCell
--         toHeaderRow l
--           | null l    = []
--           | otherwise = [toRow headers]
--         th = TableHead nullAttr (toHeaderRow headers)
--         tb = TableBody nullAttr 0 [] $ map toRow rows
--         tf = TableFoot nullAttr []

-- caption :: Maybe ShortCaption -> OrgElements -> Caption
-- caption x = Caption x . toList

-- simpleCaption :: OrgElements -> Caption
-- simpleCaption = caption Nothing

-- emptyCaption :: Caption
-- emptyCaption = simpleCaption mempty

-- simpleFigureWith :: Attr -> OrgInlines -> Text -> Text -> OrgElements
-- simpleFigureWith attr figureCaption url title =
--   para $ imageWith attr url ("fig:" <> title) figureCaption

-- simpleFigure :: OrgInlines -> Text -> Text -> OrgElements
-- simpleFigure = simpleFigureWith nullAttr

-- divWith :: Attr -> OrgElements -> OrgElements
-- divWith attr = one . Div attr . toList
