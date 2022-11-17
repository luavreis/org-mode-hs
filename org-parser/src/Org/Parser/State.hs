module Org.Parser.State where

import Org.Types

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

defaultOrgOptions :: OrgOptions
defaultOrgOptions =
  OrgOptions
    { orgSrcPreserveIndentation = False,
      orgSrcTabWidth = 4,
      orgTodoKeywords = [TodoKeyword Todo "TODO", TodoKeyword Todo "DONE"],
      orgElementParsedKeywords = ["caption"],
      orgElementDualKeywords = ["caption", "results"],
      orgElementAffiliatedKeywords = ["caption", "data", "header", "headers", "label", "name", "plot", "resname", "result", "source", "srcname", "tblname"]
    }

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStatePendingAffiliated :: [(Text, AffKeywordValue)],
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
