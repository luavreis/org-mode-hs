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
import Org.Exporters.Processing.OrgData (OrgData)
import Org.Types
import System.FilePath
import Text.XmlHtml qualified as X

defHtmlBackend :: Monad m => ExportBackend m
defHtmlBackend =
  let affiliatedMap kws =
        "affiliated" ## const $ pure affAttrs
        where
          affAttrs :: [(Text, Text)]
          affAttrs = join $ mapMaybe getHtmlAttrs (Map.toList kws)
          getHtmlAttrs ("html", BackendKeyword v) = Just v
          getHtmlAttrs ("name", ValueKeyword v) = Just [("id", v)]
          getHtmlAttrs _ = Nothing
      srcPretty _ _ _ = namespace pass
      babelCall _ = namespace pass
      macro _ _ = namespace pass
      customElement _ = Nothing
      customObject _ = Nothing
   in ExportBackend {..}

loadLayout :: FilePath -> IO X.Document
loadLayout dir = do
  let file = dir </> "document.tpl"
  text <- readFileBS file
  case X.parseHTML file text of
    Left s -> throw (TemplateLoadingException s)
    Right t -> pure t

render' :: X.Document -> LByteString
render' = toLazyByteString . X.render

render ::
  Monad m =>
  OndimState m ->
  Ondim m X.Document ->
  m (Either OndimException LByteString)
render st spl =
  evalOndimTWith st spl
    <&> fmap render'

renderFragment' :: [HtmlNode] -> LByteString
renderFragment' = toLazyByteString . X.renderHtmlFragment X.UTF8 . toNodeList

renderFragment ::
  Monad m =>
  OndimState m ->
  Ondim m [HtmlNode] ->
  m (Either OndimException LByteString)
renderFragment st spl =
  evalOndimTWith st spl <&> fmap renderFragment'

renderDoc ::
  Monad m =>
  ExportBackend m ->
  OndimState m ->
  X.Document ->
  OrgData ->
  OrgDocument ->
  m (Either OndimException LByteString)
renderDoc bk st layout datum doc =
  liftSubstructures layout
    `binding` documentExp bk datum doc
    & bindDefaults
    & evalOndimTWith st
    <&> fmap render'
