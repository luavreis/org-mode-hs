{-# LANGUAGE OverloadedLists #-}

module Org.Parser.State where

import Data.Aeson qualified as Aeson
import Org.Types

-- | Collection of todo markers in the order in which items should progress
type TodoSequence = [TodoKeyword]

data OrgOptions = OrgOptions
  { orgSrcPreserveIndentation :: Bool
  , orgSrcTabWidth :: Int
  , orgTodoKeywords :: TodoSequence
  , orgElementParsedKeywords :: Set Text
  , orgElementDualKeywords :: Set Text
  , orgElementAffiliatedKeywords :: Set Text
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Aeson.ToJSON OrgOptions where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance Aeson.FromJSON OrgOptions where
  parseJSON = Aeson.genericParseJSON aesonOptions

instance NFData OrgOptions

defaultOrgOptions :: OrgOptions
defaultOrgOptions =
  OrgOptions
    { orgSrcPreserveIndentation = False
    , orgSrcTabWidth = 4
    , orgTodoKeywords = [TodoKeyword Todo "TODO", TodoKeyword Todo "DONE"]
    , orgElementParsedKeywords = ["caption", "title", "date", "author"]
    , orgElementDualKeywords = ["caption", "results"]
    , orgElementAffiliatedKeywords = ["caption", "data", "header", "headers", "label", "name", "plot", "resname", "result", "source", "srcname", "tblname"]
    }

-- | Org-mode parser state
data OrgParserEnv = OrgParserEnv
  { orgEnvOptions :: OrgOptions
  , orgEnvIndentLevel :: Int
  }

-- | Org-mode parser state
newtype OrgParserState = OrgParserState
  { orgStateLastChar :: Maybe Char
  }

defaultState :: OrgParserState
defaultState =
  OrgParserState
    { orgStateLastChar = Nothing
    }

defaultEnv :: OrgParserEnv
defaultEnv =
  OrgParserEnv
    { orgEnvOptions = defaultOrgOptions
    , orgEnvIndentLevel = 0
    }
