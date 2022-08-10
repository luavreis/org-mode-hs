-- |

module Main where
import Options.Applicative
import Org.Parser
import Org.Exporters.Ondim
import Org.Exporters.Common
import Org.Types

filepath :: Parser FilePath
filepath = strOption
           ( long "input"
           <> short 'i'
           )

main :: IO ()
main = do
  d <- parseOrgIO defaultOrgOptions =<< execParser opts
  st <- loadOrgTemplates
  either print putBS $
    second toStrict $
    renderExpansible defaultExporterSettings st (documentSections d)
  where
    opts = info (filepath <**> helper)
           ( fullDesc
           <> progDesc "Test test "
           <> header "org-parser - parse your Org documents."
           )
