{-# LANGUAGE RecordWildCards #-}

module Org.Exporters.LaTeX where

import Control.Exception (throwIO)
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Targets.Whiskers
import Org.Exporters.Common
import Org.Exporters.Processing.OrgData (OrgData, initialOrgData)
import Org.Types
import System.FilePath

type LaTeXBackend m = ExportBackend m Node Node

defLaTeXBackend :: Monad m => LaTeXBackend m
defLaTeXBackend =
  let nullObj = Textual ""
      plain = one . Textual -- TODO protect
      softbreak = [Textual "\n"]
      exportSnippet "latex" = one . Textual
      exportSnippet _ = const []
      nullEl = nullObj
      affiliatedMap _ = pure ()
      srcPretty _ _ _ = pure Nothing
      rawBlock "latex" = one . Textual
      rawBlock _ = const []
      plainObjsToEls = id
      stringify = mempty
      inlBabelCall _ = pure []
      macro _ _ = pure []
      customElement _ = Nothing
      customObject _ = Nothing
   in ExportBackend {..}

laTeXTemplateDir :: IO FilePath
laTeXTemplateDir = (</> "latex") <$> templateDir

loadLayout :: FilePath -> IO [Node]
loadLayout dir = do
  let file = dir </> "org/document.tex"
  text <- parseWhiskers ("<<", ">>") file . decodeUtf8 <$> readFileBS file
  either (throwIO . TemplateLoadingException) pure text

evalOndim ::
  Monad m =>
  OndimState m ->
  OrgData ->
  Ondim m a ->
  m (Either OndimException a)
evalOndim st eSt spl =
  spl
    & bindDefaults
    & evalOndimTWith st
    & flip runReaderT eSt

render ::
  Monad m =>
  OndimState m ->
  OrgData ->
  Ondim m [Node] ->
  m (Either OndimException LByteString)
render st eSt spl =
  evalOndim st eSt spl
    <&> fmap (encodeUtf8 . renderWhiskers)

renderDoc ::
  Monad m =>
  LaTeXBackend m ->
  OndimState m ->
  [Node] ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk st layout datum doc =
  evalOndim st initialOrgData (bindDocument bk datum doc (liftNodes layout))
    <&> fmap (encodeUtf8 . renderWhiskers)
