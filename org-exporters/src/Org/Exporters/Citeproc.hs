module Org.Exporters.Citeproc where

import Citeproc hiding (Citation, toText)
import Citeproc qualified as C
import Citeproc.CslJson
import Data.Aeson (decode)
import Org.Data.Entities (defaultEntitiesMap, utf8Replacement)
import Org.Types
import Relude.Extra (lookup)

toCslJson :: [OrgObject] -> CslJson Text
toCslJson [] = CslEmpty
toCslJson (x : xs) = to x <> toCslJson xs
  where
    to (Plain t) = CslText t
    to SoftBreak = CslText " "
    to LineBreak = CslText " "
    to (Italic o) = CslItalic $ toCslJson o
    to (Underline o) = CslUnderline $ toCslJson o
    to (Bold o) = CslBold $ toCslJson o
    to (Superscript o) = CslSup $ toCslJson o
    to (Subscript o) = CslSub $ toCslJson o
    to (Strikethrough o) = CslDiv "striketrough" $ toCslJson o
    to (Quoted _ o) = CslQuoted $ toCslJson o
    to (Verbatim o) = CslNoDecoration $ CslText o
    to (Code o) = CslNoDecoration $ CslText o
    to (Src _ _ o) = CslNoDecoration $ CslText o
    to (Entity e)
      | Just t <- lookup e defaultEntitiesMap = CslText (utf8Replacement t)
      | otherwise = CslEmpty
    to (Link _ c) = toCslJson c
    to (LaTeXFragment InlMathFragment m) = CslDiv "math inline" $ CslText $ "\\(" <> m <> "\\)"
    to (LaTeXFragment DispMathFragment m) = CslDiv "math display" $ CslText $ "\\[" <> m <> "\\]"
    to (LaTeXFragment RawFragment m) = CslDiv "math raw" $ CslText m
    to ExportSnippet {} = CslEmpty
    to FootnoteRef {} = CslEmpty
    to Timestamp {} = CslEmpty
    to Cite {} = CslEmpty
    to InlBabelCall {} = CslEmpty
    to Macro {} = CslEmpty
    to Target {} = CslEmpty

data CiteStyle
  = Author
  | NoAuthor
  | Text
  | Default

-- data CiteVariant
--   = Caps
--   | Full
--   | Bare

getCiteStyle :: Citation -> CiteStyle
getCiteStyle Citation {citationStyle = sty} = style sty
  where
    style :: Text -> CiteStyle
    style s
      | s == "a" || s == "author" = Author
      | s == "na" || s == "noauthor" = NoAuthor
      | s == "t" || s == "text" = Text
      | otherwise = Default

-- variant :: Text -> Maybe CiteVariant
-- variant v | v == "c" || v == "caps" = Just Caps
--           | v == "f" || v == "full" = Just Full
--           | v == "b" || v == "bare" = Just Bare
--           | otherwise = Nothing

citeToCiteproc :: Citation -> C.Citation (CslJson Text)
citeToCiteproc cite =
  C.Citation Nothing Nothing $
    foldMap refToItem (citationReferences cite)
      & changeFst
        ( \c ->
            c
              { citationItemPrefix =
                  liftA2
                    (<>)
                    (toCslJson <$> nonEmpty' (citationPrefix cite))
                    (citationItemPrefix c)
              }
        )
      & changeLast
        ( \c ->
            c
              { citationItemSuffix =
                  liftA2
                    (<>)
                    (citationItemSuffix c)
                    (toCslJson <$> nonEmpty' (citationSuffix cite))
              }
        )
  where
    refToItem :: CiteReference -> [C.CitationItem (CslJson Text)]
    refToItem ref = case getCiteStyle cite of
      Text ->
        [ (citeConstr AuthorOnly) {citationItemSuffix = Nothing},
          (citeConstr SuppressAuthor) {citationItemPrefix = Nothing}
        ]
      Author -> [citeConstr AuthorOnly]
      NoAuthor -> [citeConstr SuppressAuthor]
      Default -> [citeConstr NormalCite]
      where
        citeConstr kind =
          CitationItem
            (ItemId $ refId ref)
            Nothing -- TODO what is this exactly?
            Nothing
            kind
            (Just $ toCslJson $ refPrefix ref)
            (Just $ toCslJson $ refSuffix ref)
            Nothing
    changeLast _ [] = []
    changeLast f [x] = [f x]
    changeLast f (x : xs) = x : changeLast f xs

    changeFst _ [] = []
    changeFst f (x : xs) = f x : xs

    nonEmpty' [] = Nothing
    nonEmpty' x = Just x

-- processCitations ::
--   Walkable OrgObject a =>
--   CiteprocOptions ->
--   Style (CslJson Text) ->
--   Maybe Lang ->
--   [Reference (CslJson Text)] ->
--   a ->
--   (a, [Text], [Text])
-- processCitations opt sty lang refs doc =
--   (,map (render . snd) (resultBibliography result),resultWarnings result) $
--     flip evalState (resultCitations result) $
--       flip walkM doc \case
--         c@(Cite _) -> do
--           get >>= \case
--             x : xs -> do put xs; pure $ ExportSnippet "html" (render x)
--             [] -> pure c
--         x -> pure x
--   where
--     citations = flip query doc $ \case
--       Cite cite -> [citeToCiteproc cite]
--       _ -> []
--     result = citeproc opt sty lang refs citations
--     locale = fromMaybe mempty $ case lang of
--       l@(Just _) -> find ((l ==) . localeLanguage) (styleLocales sty)
--       Nothing -> Nothing
--     render = renderCslJson True locale

loadStyle :: MonadIO m => FilePath -> m (Style (CslJson Text))
loadStyle fp = do
  xml <- readFileBS fp
  parseStyle (\_ -> pure "") (decodeUtf8 xml) >>= \case
    Left e -> error $ prettyCiteprocError e
    Right s -> pure s

loadBibliography :: MonadIO m => FilePath -> m [Reference (CslJson Text)]
loadBibliography fp = do
  json <- readFileLBS fp
  case decode json of
    Just r -> pure r
    Nothing -> error $ "Could not parse CSL JSON bibliography at " <> toText fp

-- TODO: the code below is very bad. Handling should happen during export!
-- processCitationsInDoc ::
--   MonadIO m =>
--   CiteprocOptions ->
--   FilePath ->
--   [FilePath] ->
--   Maybe FilePath ->
--   OrgDocument ->
--   m OrgDocument
-- processCitationsInDoc cpOpt root ocGlobalBib defStyle doc = do
--   let bibFp = (ocGlobalBib ++) $
--               lookupKeyword "bibliography" doc
--               & mapMaybe justText
--               & map (makeAbsolute . toString)
--       styFp = (lookupKeyword "cite_export" doc
--                & mapMaybe justText
--                & find (maybe False (("" /=) . T.strip) . T.stripPrefix "csl ")
--                & fmap (makeAbsolute . toString))
--               <|> defStyle
--       lang = case lookupKeyword "language" doc
--                   & mapMaybe justText
--                   & viaNonEmpty head
--                   & fmap parseLang of
--                Just (Right l) -> Just l
--                _ -> Nothing
--   bib <- foldMapM (loadBibliography . toString) bibFp
--   sty <- loadStyle (maybe (error "No CSL style supplied for citations!") toString styFp)
--   let (doc', refs, _errs) = processCitations cpOpt sty lang bib doc
--       bibSection =
--         PlainList
--           (M.singleton "attr_html" (BackendKeyword [("class","csl-bib-body")]))
--           (Unordered '-')
--           (ListItem (Bullet '-') Nothing Nothing [] . one . ExportBlock "html" <$> refs)
--       doc'' = flip walk doc' \case
--         Keyword "print_bibliography" _ -> bibSection
--         x -> x
--   pure doc''
--   where
--     makeAbsolute fp | isAbsolute fp = root </> fp
--                     | otherwise = fp
--     justText (ValueKeyword _ t) = Just t
--     justText _ = Nothing
