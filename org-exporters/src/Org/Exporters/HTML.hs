{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.HTML where

import Control.Exception (throwIO)
import Data.ByteString.Builder (toLazyByteString)
import Ondim
import Ondim.Extra
import Ondim.HTML
import Org.Exporters.Common
import Org.Types
import System.Directory.Recursive
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

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show, Exception)

htmlTemplateDir :: IO FilePath
htmlTemplateDir = (</> "templates/html") <$> templateDir

loadTemplates :: FilePath -> IO (OndimS HTag HtmlNode)
loadTemplates dir = do
  files <- getFilesRecursive dir
  templates <- forM files $ \file -> do
    let name = takeBaseName file
    text <- readFileBS file
    case X.parseHTML file text of
      Left s -> throwIO (TemplateLoadingException s)
      Right t -> pure (fromString name, fromDocument t)
  pure $
    OndimS
      { expansions = fromList templates,
        filters = mempty
      }

loadLayout :: FilePath -> IO X.Document
loadLayout dir = do
  let file = dir </> "org:document.tpl"
  text <- readFileBS file
  case X.parseHTML file text of
    Left s -> throwIO (TemplateLoadingException s)
    Right t -> pure t

render ::
  ExporterSettings ->
  OndimS HTag HtmlNode ->
  Ondim HTag [HtmlNode] ->
  Either OndimException LByteString
render exst st spl =
  spl
    & bindDefaults
    & withOndimS (const st)
    & runOndimT
    & flip evalState defaultExporterState {exporterSettings = exst}
    & second toNodeList
    <&> X.renderHtmlFragment X.UTF8
    <&> toLazyByteString

renderDoc ::
  ExporterSettings ->
  OndimS HTag HtmlNode ->
  X.Document ->
  OrgDocument ->
  Either OndimException LByteString
renderDoc s st layout doc =
  liftDocument doc layout
    & bindDefaults
    & withOndimS (const st)
    & runOndimT
    & flip evalState st'
    <&> X.render
    <&> toLazyByteString
  where
    st' = defaultExporterState { exporterSettings = s }
