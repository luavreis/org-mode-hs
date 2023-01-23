module Org.Exporters.Highlighting.Skylighting where

import Ondim.Targets.HTML (HtmlNode, fromNodeList)
import Org.Types (AffKeywords)
import Skylighting
import Text.Blaze.Renderer.XmlHtml qualified as X

skylightingHtml :: Monad m => AffKeywords -> Text -> Text -> m (Maybe [[HtmlNode]])
skylightingHtml _ lang code = pure $ do
  let lang' = case lang of
        "C" -> "c"
        "C++" -> "cpp"
        "emacs-lisp" -> "commonlisp"
        "js" -> "javascript"
        "R" -> "r"
        "sh" -> "bash"
        "sqlite" -> "sql"
        _ -> lang
  stx <- lookupSyntax lang' defaultSyntaxMap
  let tokenizeOpts =
        TokenizerConfig
          { syntaxMap = defaultSyntaxMap,
            traceOutput = False
          }
  tokenLines <- rightToMaybe $ tokenize tokenizeOpts stx code
  pure $
    fromNodeList
      . X.renderHtmlNodes
      . formatHtmlInline defaultFormatOpts
      . one
      <$> tokenLines
