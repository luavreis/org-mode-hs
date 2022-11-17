module Main where

import Data.Text.IO qualified as T
import Ondim.Extra.Loading.HTML qualified as H
import Ondim.Extra.Loading.Pandoc qualified as P
import Options qualified as O
import Options.Applicative
import Org.Exporters.Common (templateDir)
import Org.Exporters.HTML qualified as H
import Org.Exporters.Pandoc qualified as P
import Org.Exporters.Processing (runPipeline, withCurrentData)
import Org.Exporters.Processing.InternalLinks (resolveLinks)
import Org.Exporters.Processing.Prune (pruneDoc)
import Org.Parser
import Path (parseAbsDir, parseRelDir)
import Path.IO (copyDirRecur', doesDirExist)
import System.FilePath ((</>))
import Text.Megaparsec (errorBundlePretty)
import Text.Pandoc qualified as TP
import Text.Pretty.Simple
import Org.Exporters.Processing.GatherKeywords (gatherKeywords)

main :: IO ()
main = do
  ddir <- parseAbsDir =<< templateDir
  tdir <- parseRelDir ".horg"
  udir <-
    ifM
      (doesDirExist tdir)
      (pure $ Just ".horg")
      (pure Nothing)
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
        case parseOrg defaultOrgOptions fp txt of -- TODO org options
          Left e -> do
            putTextLn
              "There was an error parsing your Org file.\n\
              \Since this should not happen, please open an issue on GitHub."
            putStrLn $ errorBundlePretty e
            error "This should not happen."
          Right d -> pure d
      let (processed, datum) = runPipeline do
            gatherKeywords parsed
            pruned <- withCurrentData $ pruneDoc parsed
            getCompose $ resolveLinks pruned
      out <-
        case O.backend opts of
          O.AST False -> pure $ show processed
          O.AST True -> pure $ encodeUtf8 $ pShowNoColor processed
          O.HTML oo -> do
            let usrDir = O.templateDir oo
                lclDir = (</> "html") <$> udir
            defDir <- H.htmlTemplateDir
            tpls <- H.loadTemplates $ catMaybes [usrDir, lclDir, Just defDir]
            layout <-
              maybe empty H.loadLayout (usrDir <|> lclDir)
                <|> H.loadLayout defDir
            either (error . show) toStrict
              <$> H.renderDoc H.defHtmlBackend datum tpls layout processed
          O.Pandoc fmt tplo oo -> do
            let usrDir = O.templateDir oo
                lclDir = (</> "pandoc") <$> udir
            defDir <- P.pandocTemplateDir
            tpls <- P.loadTemplates $ catMaybes [usrDir, lclDir, Just defDir]
            layout <-
              maybe empty P.loadPandocDoc (usrDir <|> lclDir)
                <|> P.loadPandocDoc defDir
            doc <-
              either (error . show) id
                <$> P.renderDoc P.defPandocBackend datum tpls layout processed
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
