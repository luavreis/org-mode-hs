module Org.Exporters.Highlighting.EngraveFaces where

import Data.Text qualified as T
import Ondim.Targets.HTML
import Org.Types (Keywords)
import System.Process
import Text.XmlHtml (docContent, parseHTML)
import Text.XmlHtml qualified as X

engraveFacesHtml ::
  MonadIO m =>
  Keywords ->
  Text ->
  Text ->
  m (Maybe [[HtmlNode]])
engraveFacesHtml _ (toString -> lang) code = do
  modeQ <-
    liftIO $
      readProcess
        "emacsclient"
        [ "-e",
          "(progn\
          \ (require 'org-src)\
          \ (org-src-get-lang-mode "
            ++ show lang
            ++ "))"
        ]
        ""
  let mode = case modeQ of
        (viaNonEmpty init -> Just s) -> "(" ++ s ++ ")"
        _ -> error $ "should never happen: " <> toText modeQ
  out <-
    liftIO $
      readProcess
        "emacsclient"
        [ "-e",
          "(with-temp-buffer\
          \ (require 'engrave-faces)\
          \ (insert "
            ++ show code
            ++ ") "
            ++ mode
            ++ " (setq-local engrave-faces-html-output-style nil)\
               \ (with-current-buffer (engrave-faces-html-buffer 'doom-tomorrow-day)\
               \ (prog1 (buffer-string) (kill-buffer))))"
        ]
        ""
  pure do
    htmlText :: ByteString <- readMaybe out
    nodes <- fmap docContent $ rightToMaybe $ parseHTML "" htmlText
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
        insert x (y : ys) = (x : y) : ys
        insert x [] = [[x]]
        append [] ys = ys
        append [x] (y : ys) = (x : y) : ys
        append (x : xs) ys = [x] : append xs ys
