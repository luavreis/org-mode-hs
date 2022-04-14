-- |

module Org.Parser
  ( defaultOrgOptions
  , OrgOptions
  , parseOrg
  , parseOrgIO
  ) where
import Org.Parser.Document
import Org.Parser.State
import Org.Parser.Definitions

parseOrg :: OrgOptions -> FilePath -> Text -> Either OrgParseError OrgDocument
parseOrg opt = parse (evalStateT orgDocument defaultState { orgStateOptions = opt })

parseOrgIO :: MonadIO m => OrgOptions -> FilePath -> m OrgDocument
parseOrgIO opt fp = do
  out <- parseOrg opt fp <$> readFileText fp
  case out of
    Left e -> error . toText $ errorBundlePretty e
    Right d -> pure d
