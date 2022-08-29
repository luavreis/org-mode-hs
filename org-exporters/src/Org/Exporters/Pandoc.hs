{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.Pandoc where

import Control.Exception (throw)
import Ondim
import Ondim.Pandoc
import Org.Exporters.Common
import Org.Types (OrgDocument)
import System.FilePath
import Text.Pandoc (def, readerExtensions, renderError, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Ondim.Extra.Loading (TemplateLoadingError(..))

type PTag = PandocTag (State ExporterState)

instance ExportBackend PTag where
  type ObjectNode PTag = P.Inline
  nullObj = P.Str ""
  plain = toList . B.text
  softbreak = [P.SoftBreak]
  exportSnippet l = one . P.RawInline (P.Format l)
  type ElementNode PTag = P.Block
  nullEl = P.Null
  rawBlock l = one . P.RawBlock (P.Format l)
  hN level = fmap $ one . adjLevel level
    where
      adjLevel i (P.Header _ x z) = P.Header i x z
      adjLevel _ x = x
  mergeLists = (foldr go [] <$>)
    where
      go :: P.Block -> [P.Block] -> [P.Block]
      go (P.BulletList x) (P.BulletList y : r) = P.BulletList (x ++ y) : r
      go (P.OrderedList a x) (P.OrderedList b y : r) | a == b = P.OrderedList a (x ++ y) : r
      go (P.DefinitionList x) (P.DefinitionList y : r) = P.DefinitionList (x ++ y) : r
      go x y = x : y
  plainObjsToEls = one . P.Plain
  stringify = Ondim.Pandoc.stringify
  type DocumentNode PTag = P.Pandoc

pandocTemplateDir :: IO FilePath
pandocTemplateDir = (</> "pandoc") <$> templateDir

loadPandocDoc :: FilePath -> IO P.Pandoc
loadPandocDoc dir = do
  let file = dir </> "org/document.md"
  text :: Text <- decodeUtf8 <$> readFileBS file
  let pandoc =
        runPure $
          readMarkdown def {readerExtensions = pandocExtensions} text
  case pandoc of
    Left s -> throw (TemplateLoadingException (toString $ renderError s))
    Right t -> pure t

renderDoc ::
  ExporterSettings ->
  OndimMS PTag ->
  P.Pandoc ->
  OrgDocument ->
  Either OndimException P.Pandoc
renderDoc s st layout doc =
  liftDocument doc layout
    & bindDefaults
    & runOndimTWith st
    & flip evalState st'
  where
    st' = defaultExporterState {exporterSettings = s}
