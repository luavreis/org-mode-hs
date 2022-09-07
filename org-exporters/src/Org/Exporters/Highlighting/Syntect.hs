{-# LANGUAGE ForeignFunctionInterface #-}

module Org.Exporters.Highlighting.Syntect where

import Data.Text qualified as T
import Foreign.C.String
import Ondim.HTML
import Text.XmlHtml (docContent, parseHTML)
import Text.XmlHtml qualified as X
import Control.Exception (bracket)
import Org.Parser.Definitions (Affiliated)

foreign import ccall unsafe "render_lines" renderLines :: CString -> CString -> IO CString
foreign import ccall unsafe "free_lines" freeLines :: CString -> IO ()

synctectSrcLinesHtml :: MonadIO m => Affiliated -> Text -> Text -> m (Maybe [[HtmlNode]])
synctectSrcLinesHtml _ (toString -> lang) (toString -> code) = do
  let lang' = case lang of
        "emacs-lisp" -> "lisp"
        _ -> lang
  out :: String <-
    liftIO $
      withCString lang' \lPtr ->
        withCString code \cPtr ->
          bracket (renderLines lPtr cPtr) freeLines peekCString
  pure do
    nodes <- fmap docContent $ rightToMaybe $ parseHTML "" (encodeUtf8 out)
    pure $ fromNodeList <$> breakLines nodes
  where
    breakLines :: [X.Node] -> [[X.Node]]
    breakLines = go
      where
        go [] = [[]]
        go (X.TextNode t : xs) =
          let ts = map X.TextNode (T.split (== '\n') t)
           in append ts (go xs)
        go (X.Element x y c : ns) =
          let els = map (X.Element x y) (go c)
           in append els (go ns)
        go (n : ns) = insert n (go ns)
        insert x (y : ys) = ((x : y) : ys)
        insert x [] = [[x]]
        append [] ys = ys
        append [x] (y : ys) = ((x : y) : ys)
        append (x : xs) ys = [x] : append xs ys
