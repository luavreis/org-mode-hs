-- |

module Main where
import Options.Applicative
import Org.Parser

filepath :: Parser FilePath
filepath = strOption
           ( long "input"
           <> short 'i'
           )

main :: IO ()
main = print =<< parseOrgIO defaultOrgOptions =<< execParser opts
  where
    opts = info (filepath <**> helper)
           ( fullDesc
           <> progDesc "Test test "
           <> header "org-parser - parse your Org documents."
           )
