{-# LANGUAGE RecordWildCards #-}

module Org.Exporters.LaTeX where

import Control.Exception (throwIO)
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Targets.Whiskers
import Org.Exporters.Common
import Org.Exporters.Processing.OrgData (OrgData)
import Org.Types
import System.FilePath

defLaTeXBackend :: Monad m => ExportBackend m
defLaTeXBackend =
  let affiliatedMap _ = pure ()
      customElement _ = Nothing
      customObject _ = Nothing
      srcPretty _ _ _ = namespace pass
      babelCall _ = namespace pass
      macro _ _ = namespace pass
   in ExportBackend {..}

laTeXTemplateDir :: IO FilePath
laTeXTemplateDir = (</> "latex") <$> templateDir

loadLayout :: FilePath -> IO [Node]
loadLayout dir = do
  let file = dir </> "org/document.tex"
  text <- parseWhiskers ("<<", ">>") file . decodeUtf8 <$> readFileBS file
  either (throwIO . TemplateLoadingException) pure text

render ::
  Monad m =>
  OndimState m ->
  Ondim m [Node] ->
  m (Either OndimException LByteString)
render st spl =
  evalOndimTWith st spl
    <&> fmap (encodeUtf8 . renderWhiskers)

renderDoc ::
  Monad m =>
  ExportBackend m ->
  OndimState m ->
  [Node] ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk st layout datum doc =
  liftNodes layout
    `binding` documentExp bk datum doc
    & bindDefaults
    & evalOndimTWith st
    <&> fmap (encodeUtf8 . renderWhiskers)
