{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import Data.Text.IO qualified as T
import Ondim
import Ondim.Debug (OndimException)
import Ondim.Extra.Exceptions (prettyException)
import Ondim.Loading (LoadConfig (..), TemplateLoadingException (..), loadTemplates)
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
import Org.Exporters.Processing (OrgData, processAll)
import Org.Parser
import Org.Types.Variants.Annotated (OrgDocument)
import System.Directory qualified as D
import System.FilePath (isDrive, takeDirectory, (</>))

loadPandoc :: forall m. (Monad m) => LoadConfig m
loadPandoc = (P.loadPandocMd @m) {initialState = templatesEmbed [P.loadPandocMd]}

loadHtml :: forall m. (Monad m) => LoadConfig m
loadHtml = (H.loadHtml @m) {initialState = templatesEmbed [H.loadHtml]}

loadLaTeX :: forall m. (Monad m) => LoadConfig m
loadLaTeX = (L.loadLaTeX @m) {initialState = templatesEmbed [L.loadLaTeX]}

renderDoc ::
  [FilePath] ->
  O.OndimOptions ->
  OrgData ->
  OrgDocument ->
  IO (Either OndimException LByteString)
renderDoc udirs oo datum doc = do
  let dirs = maybeToList (O.templateDir oo) ++ udirs
  let (cfg, bk) = case O.format oo of
        O.HTML -> (loadHtml, H.defBackend)
        O.Pandoc -> (loadPandoc, P.defBackend)
        O.LaTeX -> (loadLaTeX, L.defBackend)
  tpls <-
    if null dirs
      then return $ initialState cfg
      else loadTemplates [cfg] dirs
  evalOndimWith tpls $
    renderTemplateOrError (O.layout oo)
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
      let parsed = parseOrgDoc defaultOrgOptions fp txt -- TODO org options
          (processed, datum) = processAll parsed
      out <- try
        case O.backend opts of
          O.AST -> return $ Right $ show processed
          O.Ondim oo -> renderDoc udirs oo datum processed
      case out of
        Right (Right out') -> case O.output opts of
          O.StdOutput -> putBSLn $ toStrict out'
          O.FileOutput f -> writeFileLBS f out'
        Right (Left s) -> putTextLn $ prettyException s
        Left (TemplateLoadingException s) -> putStrLn s
