module Main where

import Options.Applicative
import Org.Exporters.Common
import Org.Exporters.HTML
import Org.Parser
import Org.Types

filepath :: Parser FilePath
filepath =
  strOption
    ( long "input"
        <> short 'i'
    )

main :: IO ()
main = do
  d <- parseOrgIO defaultOrgOptions =<< execParser opts
  st <- loadHtmlTemplates
  either print putBS $
    second toStrict $
      render defaultExporterSettings st (expandOrgSections $ documentSections d)
  where
    opts =
      info
        (filepath <**> helper)
        ( fullDesc
            <> progDesc "Test test "
            <> header "org-parser - parse your Org documents."
        )
