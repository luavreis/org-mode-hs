module Main where

import Data.Text.IO qualified as T
import Options qualified as O
import Options.Applicative
import Org.Exporters.HTML qualified as H
import Org.Exporters.Pandoc qualified as P
import Org.Parser
import Text.Megaparsec (errorBundlePretty)
import Text.Pandoc qualified as TP
import Text.Pretty.Simple

main :: IO ()
main = do
  opts <- execParser O.appOptions
  (txt, fp) <- case O.input opts of
    O.StdInput -> (,"stdin") <$> T.getContents
    O.FileInput f -> (,f) . decodeUtf8 <$> readFileBS f
  parsed <-
    case parseOrg (O.options opts) fp txt of
      Left e -> do
        putTextLn
          "There was an error parsing your Org file.\n\
          \Since this should not happen, please open an issue on GitHub."
        putStrLn $ errorBundlePretty e
        error "This should not happen."
      Right d -> pure d
  out <-
    case O.backend opts of
      O.AST False -> pure $ show parsed
      O.AST True -> pure $ encodeUtf8 $ pShowNoColor parsed
      O.HTML oo -> do
        dt <- H.loadTemplates =<< H.templateDir
        ut <- maybe (pure mempty) H.loadTemplates (O.templateDir oo)
        layout <-
          maybe empty H.loadLayout (O.templateDir oo)
            <|> (H.loadLayout =<< H.templateDir)
        pure $
          either (error . show) toStrict $
            H.renderDoc (O.settings oo) (ut <> dt) layout parsed
      O.Pandoc fmt tplo oo -> do
        dit <- P.loadInlineTemplates =<< P.templateDir
        dbt <- P.loadBlockTemplates =<< P.templateDir
        uit <- maybe (pure mempty) P.loadInlineTemplates (O.templateDir oo)
        ubt <- maybe (pure mempty) P.loadBlockTemplates (O.templateDir oo)
        layout <-
          maybe empty P.loadPandocDoc (O.templateDir oo)
            <|> (P.loadPandocDoc =<< P.templateDir)
        let doc =
              either (error . show) id $
                P.renderDoc (O.settings oo) (uit <> dit) (ubt <> dbt) layout parsed
        TP.runIOorExplode do
          (w, ext) <- TP.getWriter fmt
          tpl <- TP.compileDefaultTemplate fmt
          utpl <- case tplo of
            Nothing -> pure Nothing
            Just tfp -> do
              tplText <- TP.getTemplate tfp
              either (const Nothing) Just <$>
                TP.runWithPartials (TP.compileTemplate tfp tplText)
          let tpl' = fromMaybe tpl utpl
              wopt = TP.def {TP.writerExtensions = ext, TP.writerTemplate = Just tpl'}
          case w of
            TP.TextWriter f -> encodeUtf8 <$> f wopt doc
            TP.ByteStringWriter f -> toStrict <$> f wopt doc
  case O.output opts of
    O.StdOutput -> putBSLn out
    O.FileOutput f -> writeFileBS f out
