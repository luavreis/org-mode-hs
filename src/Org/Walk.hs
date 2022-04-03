{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Org.Walk
  ( module Org.Walk
  , module Text.Pandoc.Walk
  ) where
import Org.Types
import Text.Pandoc.Walk (Walkable, walk, walkM, query)

instance Walkable OrgInline OrgInline where
  walkM f x = walkObjectM f x >>= f
  query f x = f x <> queryObject f x

instance Walkable OrgInline Citation where
  walkM f (Citation cs cv cpf csf cr) = do
    prefix <- walkM f cpf
    suffix <- walkM f csf
    refs   <- mapM f' cr
    pure $ Citation cs cv prefix suffix refs
    where
      f' (CiteReference i p s) = CiteReference i <$> walkM f p <*> walkM f s
  query f (Citation _ _ cpf csf cr) =
    mconcat [ query f cpf
            , query f csf
            , foldMap f' cr
            ]
    where f' (CiteReference _ p s) = query f p <> query f s

walkObjectM :: (Monad m, Walkable a OrgInline, Walkable a Citation) => (a -> m a) -> OrgInline -> m OrgInline
walkObjectM f (Italic o) = Italic <$> walkM f o
walkObjectM f (Underline o) = Underline <$> walkM f o
walkObjectM f (Bold o) = Bold <$> walkM f o
walkObjectM f (Strikethrough o) = Strikethrough <$> walkM f o
walkObjectM f (Subscript o) = Subscript <$> walkM f o
walkObjectM f (Superscript o) = Superscript <$> walkM f o
walkObjectM f (Quoted t o) = Quoted t <$> walkM f o
walkObjectM f (Link t o) = Link t <$> walkM f o
walkObjectM f (Cite c) = Cite <$> walkM f c
walkObjectM f (SpanSpecial s o) = SpanSpecial s <$> walkM f o
walkObjectM _ x@Plain {} = pure x
walkObjectM _ x@SoftBreak {} = pure x
walkObjectM _ x@LineBreak {} = pure x
walkObjectM _ x@NBSpace {} = pure x
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

queryObject :: (Monoid c, Walkable a OrgInline, Walkable a Citation) => (a -> c) -> OrgInline -> c
queryObject f (Italic o) = query f o
queryObject f (Underline o) = query f o
queryObject f (Bold o) = query f o
queryObject f (Strikethrough o) = query f o
queryObject f (Subscript o) = query f o
queryObject f (Superscript o) = query f o
queryObject f (Quoted _ o) = query f o
queryObject f (Link _ o) = query f o
queryObject f (Cite c) = query f c
queryObject f (SpanSpecial _ o) = query f o
queryObject _ Plain {} = mempty
queryObject _ SoftBreak {} = mempty
queryObject _ LineBreak {} = mempty
queryObject _ NBSpace {} = mempty
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
