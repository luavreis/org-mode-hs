{-# LANGUAGE TemplateHaskell #-}

module Org.Exporters.Data.Templates where

import Data.FileEmbed (embedDir)
import Ondim.Extra.Loading (LoadConfig, loadTemplatesEmbed)
import Org.Exporters.Common (OndimState)
import Paths_org_exporters (getDataDir)
import System.FilePath ((</>))

templateDir :: IO FilePath
templateDir = (</> "templates") <$> getDataDir

templateDirEmbeded :: [(FilePath, ByteString)]
templateDirEmbeded = $(embedDir "data/templates")

templatesEmbed :: [LoadConfig n] -> OndimState n
templatesEmbed cfg =
  loadTemplatesEmbed "org-exporters-default" cfg templateDirEmbeded
