{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.HTML where

import Control.Exception (throw)
import Data.ByteString.Builder (toLazyByteString)
import Data.Map qualified as Map
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Ondim.Extra
import Ondim.HTML
import Org.Exporters.Common
import Org.Exporters.Processing.OrgData (OrgData)
import Org.Types
import System.FilePath
import Text.XmlHtml qualified as X

type HtmlBackend m = ExportBackend HtmlTag m HtmlNode HtmlNode

defHtmlBackend :: Monad m => HtmlBackend m
defHtmlBackend =
  let nullObj = TextNode ""
      plain = one . TextNode
      softbreak = [TextNode " "]
      exportSnippet "html" = one . rawNode
      exportSnippet _ = const []
      nullEl = TextNode ""
      affiliatedEnv kws x =
        x `binding` do
          "affiliated" ## const $ pure affAttrs
        where
          affAttrs :: [(Text, Text)]
          affAttrs = join $ mapMaybe getHtmlAttrs (Map.toList kws)
          getHtmlAttrs (k, BackendKeyword v)
            | "attr_html" `T.isPrefixOf` k = Just v
          getHtmlAttrs _ = Nothing
      srcPretty _ _ _ = pure Nothing
      rawBlock "html" = one . rawNode
      rawBlock _ = const []
      mergeLists = id
      plainObjsToEls = id
      stringify = nodeText
      srcExpansionType = "html"
      srcExpansion src = do
        fromMaybe (pure []) do
          parsed <-
            rightToMaybe $
              X.parseHTML "" $ encodeUtf8 src
          pure $ liftNodes $ fromNodeList $ X.docContent parsed
      inlBabelCall _ = pure []
      macro _ _ = pure []
      customTarget = pure
   in ExportBackend {..}

-- srcExpansion src = do
--   fromMaybe (pure []) do
--     parsed <-
--       rightToMaybe $
--         X.parseHTML "" $ encodeUtf8 src
--     pure $ liftNodes $ fromNodeList $ X.docContent parsed

htmlTemplateDir :: IO FilePath
htmlTemplateDir = (</> "html") <$> templateDir

loadLayout :: FilePath -> IO X.Document
loadLayout dir = do
  let file = dir </> "org/document.tpl"
  text <- readFileBS file
  case X.parseHTML file text of
    Left s -> throw (TemplateLoadingException s)
    Right t -> pure t

evalOndim ::
  Monad m =>
  OndimMS HtmlTag m ->
  ExporterState ->
  Ondim HtmlTag m a ->
  m (Either OndimException a)
evalOndim st eSt spl =
  spl
    & bindDefaults
    & evalOndimTWith st
    & flip evalStateT eSt

render' :: X.Document -> LByteString
render' = toLazyByteString . X.render

render ::
  Monad m =>
  OndimMS HtmlTag m ->
  ExporterState ->
  Ondim HtmlTag m X.Document ->
  m (Either OndimException LByteString)
render st eSt spl =
  evalOndim st eSt spl
    <&> fmap render'

renderFragment' :: [HtmlNode] -> LByteString
renderFragment' = toLazyByteString . X.renderHtmlFragment X.UTF8 . toNodeList

renderFragment ::
  Monad m =>
  OndimMS HtmlTag m ->
  ExporterState ->
  Ondim HtmlTag m [HtmlNode] ->
  m (Either OndimException LByteString)
renderFragment st eSt spl =
  evalOndim st eSt spl <&> fmap renderFragment'

renderDoc ::
  Monad m =>
  HtmlBackend m ->
  OndimMS HtmlTag m ->
  X.Document ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk st layout datum doc =
  evalOndim st initialExporterState (liftDocument bk datum doc layout)
    <&> fmap render'
