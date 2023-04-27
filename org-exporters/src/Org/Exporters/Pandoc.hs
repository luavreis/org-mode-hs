{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Org.Exporters.Pandoc where

import Control.Exception (throw)
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Targets.Pandoc hiding (stringify)
import Ondim.Targets.Pandoc qualified as OPandoc
import Org.Exporters.Common
import Org.Types (OrgDocument)
import System.FilePath
import Text.Pandoc (def, readerExtensions, renderError, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Org.Exporters.Processing.OrgData

type PandocBackend m = ExportBackend m P.Inline P.Block

defPandocBackend :: Monad m => PandocBackend m
defPandocBackend =
  let nullObj = P.Str ""
      plain = toList . B.text
      softbreak = [P.SoftBreak]
      exportSnippet l = one . P.RawInline (P.Format l)
      nullEl = P.Plain []
      affiliatedMap _ = pure ()
      rawBlock l = one . P.RawBlock (P.Format l)
      srcPretty _ _ _ = pure Nothing
      plainObjsToEls = one . P.Plain
      inlBabelCall _ = pure []
      macro _ _ = pure []
      stringify = OPandoc.stringify
      customElement _ = Nothing
      customObject _ = Nothing
   in ExportBackend {..}

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
  Monad m =>
  PandocBackend m ->
  OndimState m ->
  P.Pandoc ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException P.Pandoc)
renderDoc bk st layout datum doc =
  liftDocument bk datum doc layout
    & bindDefaults
    & evalOndimTWith st
    & flip runReaderT initialOrgData
