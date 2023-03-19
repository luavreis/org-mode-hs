module Org.Parser.State where

import Org.Types
import qualified Data.Aeson as Aeson

-- | Collection of todo markers in the order in which items should progress
type TodoSequence = [TodoKeyword]

data OrgOptions = OrgOptions
  { orgSrcPreserveIndentation :: Bool,
    orgSrcTabWidth :: Int,
    orgTodoKeywords :: TodoSequence,
    orgElementParsedKeywords :: [Text],
    orgElementDualKeywords :: [Text],
    orgElementAffiliatedKeywords :: [Text]
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
    { orgSrcPreserveIndentation = False,
      orgSrcTabWidth = 4,
      orgTodoKeywords = [TodoKeyword Todo "TODO", TodoKeyword Todo "DONE"],
      orgElementParsedKeywords = ["caption", "title", "date", "author"],
      orgElementDualKeywords = ["caption", "results"],
      orgElementAffiliatedKeywords = ["caption", "data", "header", "headers", "label", "name", "plot", "resname", "result", "source", "srcname", "tblname"]
    }

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStatePendingAffiliated :: [(Text, KeywordValue)],
    orgStateOptions :: OrgOptions,
    orgStateLastChar :: Maybe Char
  }

defaultState :: OrgParserState
defaultState =
  OrgParserState
    { orgStatePendingAffiliated = mempty,
      orgStateOptions = defaultOrgOptions,
      orgStateLastChar = Nothing
    }
