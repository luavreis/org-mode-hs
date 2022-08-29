{-# LANGUAGE FlexibleInstances #-}
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

type HTag = HtmlTag (State ExporterState)

instance ExportBackend HTag where
  type ObjectNode HTag = HtmlNode
  nullObj = TextNode ""
  plain = one . TextNode
  softbreak = [TextNode " "]
  exportSnippet "html" = one . rawNode
  exportSnippet _ = const []
  type ElementNode HTag = HtmlNode
  nullEl = TextNode ""
  rawBlock "html" = one . rawNode
  rawBlock _ = const []
  hN level y = fmap one $ Element True ("h" <> show level) <$> attributes y <*> children y
  plainObjsToEls = id
  stringify = nodeText
  type DocumentNode HTag = X.Document

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
  ExporterSettings ->
  OndimMS HTag ->
  Ondim HTag X.Document ->
  Either OndimException LByteString
render exst st spl =
  spl
    & bindDefaults
    & runOndimTWith st
    & flip evalState st'
    <&> X.render
    <&> toLazyByteString
  where
    st' = defaultExporterState {exporterSettings = exst}

renderFragment ::
  ExporterSettings ->
  OndimMS HTag ->
  Ondim HTag [HtmlNode] ->
  Either OndimException LByteString
renderFragment exst st spl =
  spl
    & bindDefaults
    & runOndimTWith st
    & flip evalState st'
    & second toNodeList
    <&> X.renderHtmlFragment X.UTF8
    <&> toLazyByteString
  where
    st' = defaultExporterState {exporterSettings = exst}

renderDoc ::
  ExporterSettings ->
  OndimMS HTag ->
  X.Document ->
  OrgDocument ->
  Either OndimException LByteString
renderDoc s st layout doc =
  liftDocument doc layout
    & bindDefaults
    & runOndimTWith st
    & flip evalState st'
    <&> X.render
    <&> toLazyByteString
  where
    st' = defaultExporterState {exporterSettings = s}
