{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Org.Walk
  ( module Org.Walk,
    module Text.Pandoc.Walk,
  )
where

import Data.Bitraversable (bimapM)
import Org.Types
import Text.Pandoc.Walk (Walkable, query, walk, walkM)

-- * Instances

-- Objects

instance Walkable OrgObject OrgDocument where
  walkM f x = walkDocumentM f x
  query f x = queryDocument f x

instance Walkable OrgObject OrgSection where
  walkM f x = walkSectionM f x
  query f x = querySection f x

instance Walkable OrgObject OrgElement where
  walkM f x = walkElementM f x
  query f x = queryElement f x

instance Walkable OrgObject OrgObject where
  walkM f x = walkObjectM f x >>= f
  query f x = f x <> queryObject f x

instance Walkable OrgObject KeywordValue where
  walkM f x = walkKeywordM f x
  query f x = queryKeyword f x

instance Walkable OrgObject Citation where
  walkM f x = walkCitationM f x
  query f x = queryCitation f x

-- Elements

instance Walkable OrgElement OrgDocument where
  walkM f x = walkDocumentM f x
  query f x = queryDocument f x

instance Walkable OrgElement OrgSection where
  walkM f x = walkSectionM f x
  query f x = querySection f x

instance Walkable OrgElement OrgElement where
  walkM f x = walkElementM f x >>= f
  query f x = f x <> queryElement f x

instance Walkable OrgElement OrgObject where
  walkM f x = walkObjectM f x
  query f x = queryObject f x

instance Walkable OrgElement KeywordValue where
  walkM f x = walkKeywordM f x
  query f x = queryKeyword f x

instance Walkable OrgElement Citation where
  walkM f x = walkCitationM f x
  query f x = queryCitation f x

-- Sections

instance Walkable OrgSection OrgDocument where
  walkM f x = walkDocumentM f x
  query f x = queryDocument f x

instance Walkable OrgSection OrgSection where
  walkM f x = walkSectionM f x >>= f
  query f x = f x <> querySection f x

instance Walkable OrgSection OrgElement where
  walkM f x = walkElementM f x
  query f x = queryElement f x

instance Walkable OrgSection OrgObject where
  walkM f x = walkObjectM f x
  query f x = queryObject f x

instance Walkable OrgSection KeywordValue where
  walkM f x = walkKeywordM f x
  query f x = queryKeyword f x

instance Walkable OrgSection Citation where
  walkM f x = walkCitationM f x
  query f x = queryCitation f x

-- * Walking

-- Objects

walkObjectM :: (Monad m, Walkable a OrgObject, Walkable a Citation) => (a -> m a) -> OrgObject -> m OrgObject
walkObjectM f (Italic o) = Italic <$> walkM f o
walkObjectM f (Underline o) = Underline <$> walkM f o
walkObjectM f (Bold o) = Bold <$> walkM f o
walkObjectM f (Strikethrough o) = Strikethrough <$> walkM f o
walkObjectM f (Subscript o) = Subscript <$> walkM f o
walkObjectM f (Superscript o) = Superscript <$> walkM f o
walkObjectM f (Quoted t o) = Quoted t <$> walkM f o
walkObjectM f (Link t o) = Link t <$> walkM f o
walkObjectM f (Cite c) = Cite <$> walkM f c
walkObjectM _ x@Plain {} = pure x
walkObjectM _ x@SoftBreak {} = pure x
walkObjectM _ x@LineBreak {} = pure x
walkObjectM _ x@Code {} = pure x
walkObjectM _ x@Verbatim {} = pure x
walkObjectM _ x@Timestamp {} = pure x
walkObjectM _ x@Entity {} = pure x
walkObjectM _ x@LaTeXFragment {} = pure x
walkObjectM _ x@ExportSnippet {} = pure x
walkObjectM _ x@FootnoteRef {} = pure x
walkObjectM _ x@InlBabelCall {} = pure x
walkObjectM _ x@Src {} = pure x
walkObjectM _ x@Macro {} = pure x
walkObjectM _ x@Image {} = pure x
walkObjectM _ x@Target {} = pure x

queryObject :: (Monoid c, Walkable a OrgObject, Walkable a Citation) => (a -> c) -> OrgObject -> c
queryObject f (Italic o) = query f o
queryObject f (Underline o) = query f o
queryObject f (Bold o) = query f o
queryObject f (Strikethrough o) = query f o
queryObject f (Subscript o) = query f o
queryObject f (Superscript o) = query f o
queryObject f (Quoted _ o) = query f o
queryObject f (Link _ o) = query f o
queryObject f (Cite c) = query f c
queryObject _ Plain {} = mempty
queryObject _ SoftBreak {} = mempty
queryObject _ LineBreak {} = mempty
queryObject _ Code {} = mempty
queryObject _ Verbatim {} = mempty
queryObject _ Timestamp {} = mempty
queryObject _ Entity {} = mempty
queryObject _ LaTeXFragment {} = mempty
queryObject _ ExportSnippet {} = mempty
queryObject _ FootnoteRef {} = mempty
queryObject _ InlBabelCall {} = mempty
queryObject _ Src {} = mempty
queryObject _ Macro {} = mempty
queryObject _ Image {} = mempty
queryObject _ Target {} = mempty

-- Elements

walkElementM ::
  ( Monad m,
    Walkable a OrgObject,
    Walkable a OrgElement,
    Walkable a KeywordValue
  ) =>
  (a -> m a) ->
  OrgElement ->
  m OrgElement
walkElementM f (Paragraph af o) = Paragraph <$> walkM f af <*> walkM f o
walkElementM f (GreaterBlock af t o) = GreaterBlock <$> walkM f af ?? t <*> walkM f o
walkElementM f (Drawer a o) = Drawer a <$> walkM f o
walkElementM f (DynamicBlock n p o) = DynamicBlock n p <$> walkM f o
walkElementM f (PlainList af t i) = PlainList <$> walkM f af ?? t <*> mapM walkListItemM i
  where
    walkListItemM (ListItem b i' c t' e) = ListItem b i' c <$> walkM f t' <*> walkM f e
walkElementM f (ExampleBlock af i l) = ExampleBlock <$> walkM f af ?? i ?? l
walkElementM f (SrcBlock af n i p l) = SrcBlock <$> walkM f af ?? n ?? i ?? p ?? l
walkElementM f (VerseBlock af o) = VerseBlock <$> walkM f af <*> walkM f o
walkElementM f (Keyword k v) = Keyword k <$> walkM f v
walkElementM f (LaTeXEnvironment af name c) = LaTeXEnvironment <$> walkM f af ?? name ?? c
walkElementM _ x@ExportBlock {} = pure x
walkElementM _ x@Clock {} = pure x
walkElementM _ x@HorizontalRule {} = pure x

queryElement ::
  ( Monoid c,
    Walkable a OrgObject,
    Walkable a OrgElement,
    Walkable a KeywordValue
  ) =>
  (a -> c) ->
  OrgElement ->
  c
queryElement f (Paragraph af o) = query f af <> query f o
queryElement f (GreaterBlock af _ o) = query f af <> query f o
queryElement f (Drawer _ o) = query f o
queryElement f (DynamicBlock _ _ o) = query f o
queryElement f (PlainList af _ i) = query f af <> foldMap queryListItem i
  where
    queryListItem (ListItem _ _ _ t e) = query f t <> query f e
queryElement f (ExampleBlock af _ _) = query f af
queryElement f (SrcBlock af _ _ _ _) = query f af
queryElement f (VerseBlock af o) = query f af <> query f o
queryElement f (Keyword _ v) = query f v
queryElement f (LaTeXEnvironment af _ _) = query f af
queryElement _ ExportBlock {} = mempty
queryElement _ Clock {} = mempty
queryElement _ HorizontalRule {} = mempty

-- * Document

walkDocumentM ::
  ( Monad m,
    Walkable a OrgElement,
    Walkable a KeywordValue,
    Walkable a OrgSection
  ) =>
  (a -> m a) ->
  OrgDocument ->
  m OrgDocument
walkDocumentM f (OrgDocument p k fn c s) =
  OrgDocument p <$> mapM (bimapM pure (walkM f)) k <*> walkM f fn <*> walkM f c <*> walkM f s

queryDocument ::
  ( Monoid c,
    Walkable a OrgElement,
    Walkable a KeywordValue,
    Walkable a OrgSection
  ) =>
  (a -> c) ->
  OrgDocument ->
  c
queryDocument f (OrgDocument _ k fn c s) =
  query f (map snd k) <> query f fn <> query f c <> query f s

-- * Section

walkSectionM ::
  ( Monad m,
    Walkable a OrgObject,
    Walkable a OrgElement,
    Walkable a OrgSection
  ) =>
  (a -> m a) ->
  OrgSection ->
  m OrgSection
walkSectionM f (OrgSection i p t pr ttl tgs pl an c s) =
  OrgSection i p t pr <$> walkM f ttl ?? tgs ?? pl ?? an <*> walkM f c <*> walkM f s

querySection ::
  ( Monoid c,
    Walkable a OrgObject,
    Walkable a OrgElement,
    Walkable a OrgSection
  ) =>
  (a -> c) ->
  OrgSection ->
  c
querySection f (OrgSection _ _ _ _ ttl _ _ _ c s) =
  query f ttl <> query f c <> query f s

-- * KeywordValue

walkKeywordM ::
  ( Monad m,
    Walkable a OrgObject
  ) =>
  (a -> m a) ->
  KeywordValue ->
  m KeywordValue
walkKeywordM f (ParsedKeyword o c) = ParsedKeyword <$> walkM f o <*> walkM f c
walkKeywordM _ x@ValueKeyword {} = pure x
walkKeywordM _ x@BackendKeyword {} = pure x

queryKeyword ::
  ( Monoid c,
    Walkable a OrgObject
  ) =>
  (a -> c) ->
  KeywordValue ->
  c
queryKeyword f (ParsedKeyword o c) = query f o <> query f c
queryKeyword _ ValueKeyword {} = mempty
queryKeyword _ BackendKeyword {} = mempty

-- * Citation

walkCitationM ::
  ( Monad m,
    Walkable a OrgObject
  ) =>
  (a -> m a) ->
  Citation ->
  m Citation
walkCitationM f (Citation cs cv cpf csf cr) = do
  prefix <- walkM f cpf
  suffix <- walkM f csf
  refs <- mapM f' cr
  pure $ Citation cs cv prefix suffix refs
  where
    f' (CiteReference i p s) = CiteReference i <$> walkM f p <*> walkM f s

queryCitation ::
  ( Monoid c,
    Walkable a OrgObject
  ) =>
  (a -> c) ->
  Citation ->
  c
queryCitation f (Citation _ _ cpf csf cr) =
  query f cpf <> query f csf <> foldMap f' cr
  where
    f' (CiteReference _ p s) = query f p <> query f s
