{-# LANGUAGE RecordWildCards #-}

module Org.Exporters.LaTeX where

import Control.Exception (throw)
import Data.Map qualified as Map
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Targets.LaTeX
import Org.Exporters.Common
import Org.Exporters.Processing.OrgData (OrgData, initialOrgData)
import Org.Types
import System.FilePath
import Text.LaTeX.Base.Parser qualified as L
import Text.LaTeX.Base.Render qualified as L
import Text.LaTeX.Base.Syntax qualified as L

type LaTeXBackend m = ExportBackend m TeXNode TeXNode

defLaTeXBackend :: Monad m => LaTeXBackend m
defLaTeXBackend =
  let nullObj = TeXRaw ""
      plain = one . TeXRaw . L.protectText
      softbreak = [TeXRaw " "]
      exportSnippet "latex" = one . TeXRaw
      exportSnippet _ = const []
      nullEl = nullObj
      affiliatedMap kws =
        "affiliated" ## const $ pure affAttrs
        where
          affAttrs :: [(Text, Text)]
          affAttrs = join $ mapMaybe getHtmlAttrs (Map.toList kws)
          getHtmlAttrs ("html", BackendKeyword v) = Just v
          getHtmlAttrs ("name", ValueKeyword v) = Just [("id", v)]
          getHtmlAttrs _ = Nothing
      srcPretty _ _ _ = pure Nothing
      rawBlock "latex" = one . TeXRaw
      rawBlock _ = const []
      plainObjsToEls = id
      stringify = nodeText
      inlBabelCall _ = pure []
      macro _ _ = pure []
      customElement _ = Nothing
      customObject _ = Nothing
   in ExportBackend {..}

laTeXTemplateDir :: IO FilePath
laTeXTemplateDir = (</> "latex") <$> templateDir

loadLayout :: FilePath -> IO [TeXNode]
loadLayout dir = do
  let file = dir </> "org/document.tex"
  text <- L.parseLaTeXFile file
  case text of
    Left s -> throw (TemplateLoadingException (show s))
    Right t -> pure (fromHaTeX t)

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

render' :: [TeXNode] -> LByteString
render' = encodeUtf8 . L.render . toHaTeX

render ::
  Monad m =>
  OndimState m ->
  OrgData ->
  Ondim m [TeXNode] ->
  m (Either OndimException LByteString)
render st eSt spl =
  evalOndim st eSt spl
    <&> fmap render'

renderDoc ::
  Monad m =>
  LaTeXBackend m ->
  OndimState m ->
  [TeXNode] ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk st layout datum doc =
  evalOndim st initialOrgData (bindDocument bk datum doc (liftNodes layout))
    <&> fmap render'
