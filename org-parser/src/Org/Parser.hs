{-# LANGUAGE DeriveAnyClass #-}
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
import Control.Exception (throw)

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

newtype OrgParserException = OrgParserException String
  deriving (Show, Exception)

-- | Parse an Org document fully, with given options, and a filepath for error messages.
parseOrgDoc :: OrgOptions -> FilePath -> Text -> OrgDocument
parseOrgDoc opt fp txt =
  case parseOrg opt orgDocument fp txt of
    Left e -> throw $ OrgParserException (errorBundlePretty e)
    Right d -> d

-- | Parse an Org document in a UTF8 file, with given options.
parseOrgDocIO :: MonadIO m => OrgOptions -> FilePath -> m OrgDocument
parseOrgDocIO opt fp = do
  text <- readFileBS fp
  return $ parseOrgDoc opt fp $ decodeUtf8 text
