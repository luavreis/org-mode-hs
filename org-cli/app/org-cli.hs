module Main where

import Data.Text.IO qualified as T
import Options qualified as O
import Options.Applicative
import Org.Exporters.Common (templateDir)
import Org.Exporters.HTML qualified as H
import Org.Exporters.Pandoc qualified as P
import Org.Parser
import Path (parseAbsDir, parseRelDir)
import Path.IO (copyDirRecur', doesDirExist)
import Text.Megaparsec (errorBundlePretty)
import Text.Pandoc qualified as TP
import Text.Pretty.Simple
import System.FilePath ((</>))

main :: IO ()
main = do
  ddir <- parseAbsDir =<< templateDir
  tdir <- parseRelDir ".horg"
  udir <-
    ifM
      (doesDirExist tdir)
      (pure $ Just ".horg")
      (pure $ Nothing)
  execParser O.appCmd >>= \case
    O.CmdInitTemplates -> do
      ifM
        (doesDirExist tdir)
        (putStrLn "'.horg' directory already exists.")
        (copyDirRecur' ddir tdir)
    O.CmdExport opts -> do
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
            let dir = O.templateDir oo <|> (</> "html") <$> udir
            dt <- H.loadTemplates =<< H.htmlTemplateDir
            ut <- maybe (pure mempty) H.loadTemplates dir
            layout <-
              maybe empty H.loadLayout dir
                <|> (H.loadLayout =<< H.htmlTemplateDir)
            pure $
              either (error . show) toStrict $
                H.renderDoc (O.settings oo) (ut <> dt) layout parsed
          O.Pandoc fmt tplo oo -> do
            let dir = O.templateDir oo <|> (</> "pandoc") <$> udir
            dit <- P.loadInlineTemplates =<< P.pandocTemplateDir
            dbt <- P.loadBlockTemplates =<< P.pandocTemplateDir
            uit <- maybe (pure mempty) P.loadInlineTemplates dir
            ubt <- maybe (pure mempty) P.loadBlockTemplates dir
            layout <-
              maybe empty P.loadPandocDoc dir
                <|> (P.loadPandocDoc =<< P.pandocTemplateDir)
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
                  either (const Nothing) Just
                    <$> TP.runWithPartials (TP.compileTemplate tfp tplText)
              let tpl' = fromMaybe tpl utpl
                  wopt = TP.def {TP.writerExtensions = ext, TP.writerTemplate = Just tpl'}
              case w of
                TP.TextWriter f -> encodeUtf8 <$> f wopt doc
                TP.ByteStringWriter f -> toStrict <$> f wopt doc
      case O.output opts of
        O.StdOutput -> putBSLn out
        O.FileOutput f -> writeFileBS f out
