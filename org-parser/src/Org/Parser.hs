module Org.Parser
  ( defaultOrgOptions,
    OrgOptions (..),
    evalOrgMaybe,
    parseOrg,
    parseOrgIO,
  )
where

import Org.Parser.Definitions
import Org.Parser.Document

-- | Wrapper around 'parseMaybe' that evaluates the Org Parser state with the
-- desired options.
evalOrgMaybe :: OrgOptions -> OrgParser a -> Text -> Maybe a
evalOrgMaybe opt = parseMaybe . (`evalStateT` defaultState {orgStateOptions = opt})

-- | Parse an Org document fully, with given options, and a filepath for error messages.
parseOrg :: OrgOptions -> FilePath -> Text -> Either OrgParseError OrgDocument
parseOrg opt = parse (evalStateT orgDocument defaultState {orgStateOptions = opt})

-- | Parse an Org document in a UTF8 file, with given options.
parseOrgIO :: MonadIO m => OrgOptions -> FilePath -> m OrgDocument
parseOrgIO opt fp = do
  text <- readFileBS fp
  case parseOrg opt fp $ decodeUtf8 text of
    Left e -> error . toText $ errorBundlePretty e
    Right d -> pure d
