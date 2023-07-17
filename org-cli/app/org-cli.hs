{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import Data.Text.IO qualified as T
import Ondim
import Ondim.Extra.Exceptions (prettyException)
import Ondim.Extra.Loading (TemplateLoadingError (..), loadTemplates)
import Ondim.Targets.HTML qualified as H
import Ondim.Targets.LaTeX qualified as L
import Ondim.Targets.Pandoc qualified as P
import Options qualified as O
import Options.Applicative
import Org.Exporters.Common (documentExp, templateDir)
import Org.Exporters.HTML qualified as H
import Org.Exporters.LaTeX qualified as L
import Org.Exporters.Pandoc qualified as P
import Org.Exporters.Processing (OrgData, gatherSettings, runPipeline, withCurrentData)
import Org.Exporters.Processing.InternalLinks (resolveLinks)
import Org.Exporters.Processing.Prune (pruneDoc)
import Org.Parser
import Org.Types (OrgDocument)
import Path (parseAbsDir, parseRelDir)
import Path.IO (copyDirRecur', doesDirExist)
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple

renderDoc ::
  Maybe FilePath ->
  O.OndimOptions ->
  OrgData ->
  OrgDocument ->
  IO (Either OndimException LByteString)
renderDoc localDir oo datum doc = do
  defDir <- templateDir
  let usrDir = O.templateDir oo
  let (cfgs, st, bk) = case O.format oo of
        O.HTML -> ([H.loadHtml], H.defaultState, H.defBackend)
        O.Pandoc -> ([P.loadPandocMd], P.defaultState, P.defBackend)
        O.LaTeX -> ([L.loadLaTeX], L.defaultState, L.defBackend)
  tpls <- loadTemplates cfgs $ catMaybes [usrDir, localDir, Just defDir]
  evalOndimTWith (tpls <> st) $
    callTemplateFold @Rendered (O.layout oo)
      `binding` documentExp bk datum doc

main :: IO ()
main = do
  ddir <- parseAbsDir =<< templateDir
  tdir <- parseRelDir ".horg"
  ldir <-
    ifM
      (doesDirExist tdir)
      (return $ Just ".horg")
      (return Nothing)
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
        case parseOrgDoc defaultOrgOptions fp txt of -- TODO org options
          Left e -> do
            error $
              unlines
                [ "There was an error parsing your Org file."
                , "Since this should not happen, please open an issue on GitHub."
                , toText (errorBundlePretty e)
                ]
          Right d -> pure d
      let (processed, datum) = runPipeline do
            gatherSettings parsed
            pruned <- withCurrentData $ pruneDoc parsed
            getCompose $ resolveLinks pruned
      out <- try
        case O.backend opts of
          O.AST False -> return $ Right $ show processed
          O.AST True -> return $ Right $ encodeUtf8 $ pShowNoColor processed
          O.Ondim oo -> renderDoc ldir oo datum processed
      case out of
        Right (Right out') -> case O.output opts of
          O.StdOutput -> putBSLn $ toStrict out'
          O.FileOutput f -> writeFileLBS f out'
        Right (Left s) -> putTextLn $ prettyException s
        Left (TemplateLoadingException s) -> putStrLn s
