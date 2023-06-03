{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Org.Exporters.Pandoc where

import Control.Exception (throw)
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Targets.Pandoc hiding (stringify)
import Org.Exporters.Common
import Org.Types (OrgDocument)
import System.FilePath
import Text.Pandoc (def, readerExtensions, renderError, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Org.Exporters.Processing.OrgData

defPandocBackend :: Monad m => ExportBackend m
defPandocBackend =
  let affiliatedMap _ = pure ()
      customElement _ = Nothing
      customObject _ = Nothing
      srcPretty _ _ _ = namespace pass
      babelCall _ = namespace pass
      macro _ _ = namespace pass
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
  ExportBackend m ->
  OndimState m ->
  P.Pandoc ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException P.Pandoc)
renderDoc bk st layout datum doc =
  liftSubstructures layout
    `binding` documentExp bk datum doc
    & bindDefaults
    & evalOndimTWith st
