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
      hN level y = fmap one $ Element True ("h" <> show level) <$> attributes y <*> children y
      plainObjsToEls = id
      stringify = nodeText
   in ExportBackend {..}

htmlTemplateDir :: IO FilePath
htmlTemplateDir = (</> "html") <$> templateDir

loadLayout :: FilePath -> IO X.Document
loadLayout dir = do
  let file = dir </> "org/document.tpl"
  text <- readFileBS file
  case X.parseHTML file text of
    Left s -> throw (TemplateLoadingException s)
    Right t -> pure t

render ::
  Monad m =>
  OrgData ->
  OndimMS HtmlTag m ->
  Ondim HtmlTag m X.Document ->
  m (Either OndimException LByteString)
render datum st spl =
  spl
    & bindDefaults
    & runOndimTWith st
    & flip evalStateT st'
    <&> fmap X.render
    <&> fmap toLazyByteString
  where
    st' = initialExporterState {orgData = datum}

renderFragment ::
  Monad m =>
  OrgData ->
  OndimMS HtmlTag m ->
  Ondim HtmlTag m [HtmlNode] ->
  m (Either OndimException LByteString)
renderFragment datum st spl =
  spl
    & bindDefaults
    & runOndimTWith st
    & flip evalStateT st'
    <&> second toNodeList
    <&> fmap (X.renderHtmlFragment X.UTF8)
    <&> fmap toLazyByteString
  where
    st' = initialExporterState {orgData = datum}

renderDoc ::
  Monad m =>
  HtmlBackend m ->
  OrgData ->
  OndimMS HtmlTag m ->
  X.Document ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk datum st layout doc =
  liftDocument bk doc layout
    & bindDefaults
    & runOndimTWith st
    & flip evalStateT st'
    <&> fmap X.render
    <&> fmap toLazyByteString
  where
    st' = initialExporterState {orgData = datum}
