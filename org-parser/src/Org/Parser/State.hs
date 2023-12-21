{-# LANGUAGE OverloadedLists #-}

module Org.Parser.State where

import Data.Aeson qualified as Aeson
import Org.Types.Aeson (aesonOptions)
import Org.Types.Data.Section (TodoKeyword (..), TodoState (..))

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
    , orgTodoKeywords = [TodoKeyword Todo "TODO", TodoKeyword Done "DONE"]
    , orgElementParsedKeywords = ["caption", "title", "date", "author"]
    , orgElementDualKeywords = ["caption", "results"]
    , orgElementAffiliatedKeywords = ["caption", "data", "header", "headers", "label", "name", "plot", "resname", "result", "source", "srcname", "tblname"]
    }

-- | Org-mode parser state
data OrgParserEnv = OrgParserEnv
  { options :: OrgOptions
  , indentLevel :: Int
  }

-- | Org-mode parser state
newtype OrgParserState = OrgParserState
  { lastChar :: Maybe Char
  }

defaultState :: OrgParserState
defaultState =
  OrgParserState
    { lastChar = Nothing
    }

defaultEnv :: OrgParserEnv
defaultEnv =
  OrgParserEnv
    { options = defaultOrgOptions
    , indentLevel = 0
    }
