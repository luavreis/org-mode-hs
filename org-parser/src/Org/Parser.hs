module Org.Parser
  ( OrgParser
  , OrgParseError
  , OrgOptions (..)
  , TodoSequence
  , defaultOrgOptions
  , parseOrgMaybe
  , parseOrg
  , parseOrgDoc
  , parseOrgDocIO
  )
where

import Org.Parser.Definitions
import Org.Parser.Document

{- | Evaluate the Org Parser state with the desired options. Returns 'Nothing' in
   case of parse failure.
-}
parseOrgMaybe :: OrgOptions -> OrgParser a -> Text -> Maybe a
parseOrgMaybe opt p = rightToMaybe . parseOrg opt p ""

{- | Wrapper around 'parse' that evaluates the Org Parser state with the desired
   options.
-}
parseOrg :: OrgOptions -> OrgParser a -> FilePath -> Text -> Either OrgParseError a
parseOrg opt (OrgParser x) =
  parse $
    x
      `runReaderT` defaultEnv {orgEnvOptions = opt}
      `evalStateT` defaultState

-- | Parse an Org document fully, with given options, and a filepath for error messages.
parseOrgDoc :: OrgOptions -> FilePath -> Text -> Either OrgParseError OrgDocument
parseOrgDoc opt = parseOrg opt orgDocument

-- | Parse an Org document in a UTF8 file, with given options.
parseOrgDocIO :: MonadIO m => OrgOptions -> FilePath -> m OrgDocument
parseOrgDocIO opt fp = do
  text <- readFileBS fp
  case parseOrgDoc opt fp $ decodeUtf8 text of
    Left e -> error . toText $ errorBundlePretty e
    Right d -> pure d
