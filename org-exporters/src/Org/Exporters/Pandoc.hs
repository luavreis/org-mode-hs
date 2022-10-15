{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.Pandoc where

import Control.Exception (throw)
import Ondim
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Pandoc
import Org.Exporters.Common
import Org.Types (OrgDocument)
import System.FilePath
import Text.Pandoc (def, readerExtensions, renderError, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)

type PTag = PandocTag (State ExporterState)

type PandocBackend = ExportBackend PTag P.Inline P.Block

defPandocBackend :: PandocBackend
defPandocBackend =
  let nullObj = P.Str ""
      plain = toList . B.text
      softbreak = [P.SoftBreak]
      exportSnippet l = one . P.RawInline (P.Format l)
      nullEl = P.Null
      rawBlock l = one . P.RawBlock (P.Format l)
      hN level = fmap $ one . adjLevel level
        where
          adjLevel i (P.Header _ x z) = P.Header i x z
          adjLevel _ x = x
      srcPretty _ _ _ = pure Nothing
      mergeLists = (foldr go [] <$>)
        where
          go :: P.Block -> [P.Block] -> [P.Block]
          go (P.BulletList x) (P.BulletList y : r) = P.BulletList (x ++ y) : r
          go (P.OrderedList a x) (P.OrderedList b y : r) | a == b = P.OrderedList a (x ++ y) : r
          go (P.DefinitionList x) (P.DefinitionList y : r) = P.DefinitionList (x ++ y) : r
          go x y = x : y
      plainObjsToEls = one . P.Plain
      srcExpansionType = "md"
      srcExpansion src = do
        fromMaybe (pure []) do
          P.Pandoc _ parsed <-
            rightToMaybe $
              runPure $
                readMarkdown
                  def {readerExtensions = pandocExtensions}
                  src
          pure $ liftNodes @PTag parsed
   in ExportBackend {stringify = Ondim.Pandoc.stringify, ..}

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
  PandocBackend ->
  ExporterSettings ->
  OndimMS PTag ->
  P.Pandoc ->
  OrgDocument ->
  Either OndimException P.Pandoc
renderDoc bk s st layout doc =
  liftDocument bk doc layout
    & bindDefaults
    & runOndimTWith st
    & flip evalState st'
  where
    st' = defaultExporterState {exporterSettings = s}
