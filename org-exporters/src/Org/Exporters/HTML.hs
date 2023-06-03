{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.HTML where

import Control.Exception (throw)
import Data.ByteString.Builder (toLazyByteString)
import Data.Map qualified as Map
import Ondim.Extra
import Ondim.Targets.HTML
import Org.Exporters.Common
import Org.Exporters.Processing.OrgData (OrgData, initialOrgData)
import Org.Types
import System.FilePath
import Text.XmlHtml qualified as X

type HtmlBackend m = ExportBackend m HtmlNode HtmlNode

defHtmlBackend :: Monad m => HtmlBackend m
defHtmlBackend =
  let nullObj = TextNode ""
      plain = one . TextNode
      exportSnippet "html" = one . rawNode
      exportSnippet _ = const []
      nullEl = TextNode ""
      affiliatedMap kws =
        "affiliated" ## const $ pure affAttrs
        where
          affAttrs :: [(Text, Text)]
          affAttrs = join $ mapMaybe getHtmlAttrs (Map.toList kws)
          getHtmlAttrs ("html", BackendKeyword v) = Just v
          getHtmlAttrs ("name", ValueKeyword v) = Just [("id", v)]
          getHtmlAttrs _ = Nothing
      srcPretty _ _ _ = pure Nothing
      rawBlock "html" = one . rawNode
      rawBlock _ = const []
      plainObjsToEls = id
      stringify = nodeText
      inlBabelCall _ = pure []
      macro _ _ = pure []
      customElement _ = Nothing
      customObject _ = Nothing
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

render' :: X.Document -> LByteString
render' = toLazyByteString . X.render

render ::
  Monad m =>
  OndimState m ->
  OrgData ->
  Ondim m X.Document ->
  m (Either OndimException LByteString)
render st eSt spl =
  evalOndim st eSt spl
    <&> fmap render'

renderFragment' :: [HtmlNode] -> LByteString
renderFragment' = toLazyByteString . X.renderHtmlFragment X.UTF8 . toNodeList

renderFragment ::
  Monad m =>
  OndimState m ->
  OrgData ->
  Ondim m [HtmlNode] ->
  m (Either OndimException LByteString)
renderFragment st eSt spl =
  evalOndim st eSt spl <&> fmap renderFragment'

renderDoc ::
  Monad m =>
  HtmlBackend m ->
  OndimState m ->
  X.Document ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk st layout datum doc =
  evalOndim st initialOrgData (liftDocument bk datum doc layout)
    <&> fmap render'
