{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.HTML where

import Control.Exception (throw)
import Data.ByteString.Builder (toLazyByteString)
import Ondim
import Ondim.Extra
import Ondim.HTML
import Org.Exporters.Common
import Org.Types
import System.FilePath
import Text.XmlHtml qualified as X

type HTag m = HtmlTag (StateT ExporterState m)

type HtmlBackend m = ExportBackend (HTag m) HtmlNode HtmlNode

defHtmlBackend :: Monad m => HtmlBackend m
defHtmlBackend =
  let nullObj = TextNode ""
      plain = one . TextNode
      softbreak = [TextNode " "]
      exportSnippet "html" = one . rawNode
      exportSnippet _ = const []
      nullEl = TextNode ""
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
  ExporterSettings ->
  OndimMS (HTag m) ->
  Ondim (HTag m) X.Document ->
  m (Either OndimException LByteString)
render exst st spl =
  spl
    & bindDefaults
    & runOndimTWith st
    & flip evalStateT st'
    <&> fmap X.render
    <&> fmap toLazyByteString
  where
    st' = defaultExporterState {exporterSettings = exst}

renderFragment ::
  Monad m =>
  ExporterSettings ->
  OndimMS (HTag m) ->
  Ondim (HTag m) [HtmlNode] ->
  m (Either OndimException LByteString)
renderFragment exst st spl =
  spl
    & bindDefaults
    & runOndimTWith st
    & flip evalStateT st'
    <&> second toNodeList
    <&> fmap (X.renderHtmlFragment X.UTF8)
    <&> fmap toLazyByteString
  where
    st' = defaultExporterState {exporterSettings = exst}

renderDoc ::
  Monad m =>
  HtmlBackend m ->
  ExporterSettings ->
  OndimMS (HTag m) ->
  X.Document ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk s st layout doc =
  liftDocument bk doc layout
    & bindDefaults
    & runOndimTWith st
    & flip evalStateT st'
    <&> fmap X.render
    <&> fmap toLazyByteString
  where
    st' = defaultExporterState {exporterSettings = s}
