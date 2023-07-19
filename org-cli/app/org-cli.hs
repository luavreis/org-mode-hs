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
import Org.Exporters.Common (documentExp)
import Org.Exporters.Data.Templates (templateDirEmbeded, templatesEmbed)
import Org.Exporters.HTML qualified as H
import Org.Exporters.LaTeX qualified as L
import Org.Exporters.Pandoc qualified as P
import Org.Exporters.Processing (OrgData, gatherSettings, runPipeline, withCurrentData)
import Org.Exporters.Processing.InternalLinks (resolveLinks)
import Org.Exporters.Processing.Prune (pruneDoc)
import Org.Parser
import Org.Types (OrgDocument)
import System.Directory qualified as D
import System.FilePath (isDrive, takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple

pandocState :: Monad m => OndimState m
pandocState = templatesEmbed [P.loadPandocMd] <> P.defaultState

htmlState :: Monad m => OndimState m
htmlState = templatesEmbed [H.loadHtml] <> H.defaultState

latexState :: Monad m => OndimState m
latexState = templatesEmbed [L.loadLaTeX] <> L.defaultState

renderDoc ::
  [FilePath] ->
  O.OndimOptions ->
  OrgData ->
  OrgDocument ->
  IO (Either OndimException LByteString)
renderDoc udirs oo datum doc = do
  let dirs = maybeToList (O.templateDir oo) ++ udirs
  let (cfgs, st, bk) = case O.format oo of
        O.HTML -> ([H.loadHtml], htmlState, H.defBackend)
        O.Pandoc -> ([P.loadPandocMd], pandocState, P.defBackend)
        O.LaTeX -> ([L.loadLaTeX], latexState, L.defBackend)
  tpls <-
    if null dirs
      then return st
      else (<> st) <$> loadTemplates cfgs dirs
  evalOndimTWith tpls $
    callTemplateFold @Rendered (O.layout oo)
      `binding` documentExp bk datum doc

main :: IO ()
main = do
  let tdir = ".horg"
  cdir <- D.getCurrentDirectory
  let go dir st
        | isDrive dir = return st
        | otherwise = do
            let hdir = dir </> tdir
                continue = go (takeDirectory dir) st
            exists <- D.doesDirectoryExist hdir
            if exists
              then (hdir :) <$> continue
              else continue
  cfgdir <- D.getXdgDirectory D.XdgConfig "horg"
  cfgDirExists <- D.doesDirectoryExist cfgdir
  udirs <- go cdir $ bool [] [cfgdir] cfgDirExists
  execParser O.appCmd >>= \case
    O.CmdInitTemplates -> do
      exists <- D.doesDirectoryExist tdir
      if exists
        then putStrLn "'.horg' directory already exists."
        else forM_ templateDirEmbeded \(fp, bs) -> do
          let path = tdir </> fp
          D.createDirectoryIfMissing True $ takeDirectory path
          writeFileBS path bs
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
          O.Ondim oo -> renderDoc udirs oo datum processed
      case out of
        Right (Right out') -> case O.output opts of
          O.StdOutput -> putBSLn $ toStrict out'
          O.FileOutput f -> writeFileLBS f out'
        Right (Left s) -> putTextLn $ prettyException s
        Left (TemplateLoadingException s) -> putStrLn s
