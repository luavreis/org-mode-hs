{-# LANGUAGE DataKinds #-}

module Org.Exporters.Citeproc where

import Citeproc hiding (Citation, toText)
import Citeproc qualified as C
import Citeproc.CslJson
import Control.Category.Natural (type (~>) (..))
import Control.Category.RecursionSchemes qualified as R
import Data.Aeson (decode)
import Data.Ix.Foldable qualified as R
import Optics.Core ((%), (%~), _head, _last)
import Org.Data.Entities (defaultEntitiesMap, utf8Replacement)
import Org.Types.Variants.Plain
import Relude.Extra (lookup)

toCslJson :: Org ObjIx -> CslJson Text
toCslJson = getConst . (R.fold to #)
  where
    to :: ComposeIx [] OrgF (Const (CslJson Text)) ~> Const (CslJson Text)
    to = NT $ \(ComposeIx l) ->
      (`foldMap` l) \case
        OrgObjectF d -> Const $ toObj d
        OrgElementF {} -> mempty
        OrgSectionF {} -> mempty

    toObj :: OrgObjectData (Const (CslJson Text)) ObjIx -> CslJson Text
    toObj = \case
      (Plain t) -> CslText t
      LineBreak -> CslText " "
      (Italic o) -> CslItalic $ coerce o
      (Underline o) -> CslUnderline $ coerce o
      (Bold o) -> CslBold $ coerce o
      (Superscript o) -> CslSup $ coerce o
      (Subscript o) -> CslSub $ coerce o
      (Strikethrough o) -> CslDiv "striketrough" $ coerce o
      (Quoted _ o) -> CslQuoted $ coerce o
      (Verbatim t) -> CslNoDecoration $ CslText t
      (Code t) -> CslNoDecoration $ CslText t
      (Src _ _ t) -> CslNoDecoration $ CslText t
      (Entity e)
        | Just t <- lookup e defaultEntitiesMap -> CslText t.utf8Replacement
        | otherwise -> CslEmpty
      (LaTeXFragment InlMathFragment m) -> CslDiv "math inline" $ CslText $ "\\(" <> m <> "\\)"
      (LaTeXFragment DispMathFragment m) -> CslDiv "math display" $ CslText $ "\\[" <> m <> "\\]"
      (LaTeXFragment RawFragment m) -> CslDiv "math raw" $ CslText m
      x -> R.ifold x

data CiteStyle
  = Author
  | NoAuthor
  | Text
  | Default

-- data CiteVariant
--   = Caps
--   | Full
--   | Bare

-- variant :: Text -> Maybe CiteVariant
-- variant v | v == "c" || v == "caps" = Just Caps
--           | v == "f" || v == "full" = Just Full
--           | v == "b" || v == "bare" = Just Bare
--           | otherwise = Nothing

getCiteStyle :: Citation a -> CiteStyle
getCiteStyle cit = style cit.style
  where
    style :: Text -> CiteStyle
    style s
      | s == "a" || s == "author" = Author
      | s == "na" || s == "noauthor" = NoAuthor
      | s == "t" || s == "text" = Text
      | otherwise = Default

-- FIXME upstream
deriving instance (Generic (CitationItem a))

citeToCiteproc :: Citation (Org ObjIx) -> C.Citation (CslJson Text)
citeToCiteproc cite =
  C.Citation Nothing Nothing
    $ foldMap refToItem cite.references
    & (_head % #citationItemPrefix %~ ((toCslJson <$> cite.prefix) <>))
    & (_last % #citationItemPrefix %~ (<> (toCslJson <$> cite.suffix)))
  where
    refToItem :: CiteReference (Org ObjIx) -> [C.CitationItem (CslJson Text)]
    refToItem ref = case getCiteStyle cite of
      Text ->
        [ (citeConstr AuthorOnly) {citationItemSuffix = Nothing}
        , (citeConstr SuppressAuthor) {citationItemPrefix = Nothing}
        ]
      Author -> [citeConstr AuthorOnly]
      NoAuthor -> [citeConstr SuppressAuthor]
      Default -> [citeConstr NormalCite]
      where
        citeConstr kind =
          CitationItem
            (ItemId ref.id)
            Nothing -- TODO what is this exactly?
            Nothing
            kind
            (toCslJson <$> ref.prefix)
            (toCslJson <$> ref.suffix)
            Nothing

loadStyle :: (MonadIO m) => FilePath -> m (Style (CslJson Text))
loadStyle fp = do
  xml <- readFileBS fp
  parseStyle (\_ -> pure "") (decodeUtf8 xml) >>= \case
    Left e -> error $ prettyCiteprocError e
    Right s -> pure s

loadBibliography :: (MonadIO m) => FilePath -> m [Reference (CslJson Text)]
loadBibliography fp = do
  json <- readFileLBS fp
  case decode json of
    Just r -> pure r
    Nothing -> error $ "Could not parse CSL JSON bibliography at " <> toText fp
