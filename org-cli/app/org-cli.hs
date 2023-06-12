module Main where

import Control.Exception (try)
import Data.Text.IO qualified as T
import Ondim.Extra.Loading (TemplateLoadingError (..))
import Ondim.Targets.HTML.Load qualified as H
import Ondim.Targets.LaTeX.Load qualified as L
import Ondim.Targets.Pandoc.Load qualified as P
import Options qualified as O
import Options.Applicative
import Org.Exporters.Common (templateDir)
import Org.Exporters.HTML qualified as H
import Org.Exporters.LaTeX qualified as L
import Org.Exporters.Pandoc qualified as P
import Org.Exporters.Processing (gatherSettings, runPipeline, withCurrentData)
import Org.Exporters.Processing.InternalLinks (resolveLinks)
import Org.Exporters.Processing.Prune (pruneDoc)
import Org.Parser
import Path (parseAbsDir, parseRelDir)
import Path.IO (copyDirRecur', doesDirExist)
import Text.Megaparsec (errorBundlePretty)
import Text.Pandoc qualified as TP
import Text.Pandoc.Format qualified as TP
import Text.Pretty.Simple

main :: IO ()
main = do
  ddir <- parseAbsDir =<< templateDir
  tdir <- parseRelDir ".horg"
  ldir <-
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
            gatherSettings parsed
            pruned <- withCurrentData $ pruneDoc parsed
            getCompose $ resolveLinks pruned
      out <- try
        case O.backend opts of
          O.AST False -> pure $ show processed
          O.AST True -> pure $ encodeUtf8 $ pShowNoColor processed
          O.HTML oo -> do
            let usrDir = O.templateDir oo
            defDir <- templateDir
            tpls <- H.loadTemplates $ catMaybes [usrDir, ldir, Just defDir]
            layout <- H.loadLayout $ fromMaybe defDir (usrDir <|> ldir)
            either (error . show) toStrict
              <$> H.renderDoc H.defHtmlBackend tpls layout datum processed
          O.LaTeX oo -> do
            let usrDir = O.templateDir oo
            defDir <- templateDir
            tpls <- L.loadTemplates $ catMaybes [usrDir, ldir, Just defDir]
            layout <- L.loadLayout $ fromMaybe defDir (usrDir <|> ldir)
            either (error . show) toStrict
              <$> L.renderDoc L.defLaTeXBackend tpls layout datum processed
          O.Pandoc fmt tplo oo -> do
            let usrDir = O.templateDir oo
            defDir <- templateDir
            tpls <- P.loadTemplates $ catMaybes [usrDir, ldir, Just defDir]
            layout <- P.loadPandocDoc $ fromMaybe defDir (usrDir <|> ldir)
            doc <-
              either (error . show) id
                <$> P.renderDoc P.defPandocBackend tpls layout datum processed
            TP.runIOorExplode do
              fmt' <- TP.parseFlavoredFormat fmt
              (w, ext) <- TP.getWriter fmt'
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
      case out of
        Right out' -> case O.output opts of
          O.StdOutput -> putBSLn out'
          O.FileOutput f -> writeFileBS f out'
        Left (TemplateLoadingException s) -> putStrLn s
