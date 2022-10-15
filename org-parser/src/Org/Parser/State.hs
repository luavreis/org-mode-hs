module Org.Parser.State where

import Org.Builder (OrgElements, OrgObjects)
import Org.Types

type F = Ap (Reader OrgParserState)

data OrgOptions = OrgOptions
  { orgSrcPreserveIndentation :: Bool,
    orgSrcTabWidth :: Int,
    orgElementParsedKeywords :: [Text],
    orgElementDualKeywords :: [Text],
    orgElementAffiliatedKeywords :: [Text]
  }

defaultOrgOptions :: OrgOptions
defaultOrgOptions =
  OrgOptions
    { orgSrcPreserveIndentation = False,
      orgSrcTabWidth = 4,
      orgElementParsedKeywords = ["caption"],
      orgElementDualKeywords = ["caption", "results"],
      orgElementAffiliatedKeywords = ["caption", "data", "header", "headers", "label", "name", "plot", "resname", "result", "source", "srcname", "tblname"]
    }

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStatePendingAffiliated :: [F (KeywordKey, KeywordValue)],
    orgStateOptions :: OrgOptions,
    orgStateLastChar :: Maybe Char,
    orgStateKeywords :: [F (KeywordKey, KeywordValue)],
    orgStateLinkFormatters :: OrgLinkFormatters,
    orgStateTodoSequences :: [TodoSequence]
  }

defaultState :: OrgParserState
defaultState =
  OrgParserState
    { orgStatePendingAffiliated = mempty,
      orgStateOptions = defaultOrgOptions,
      orgStateLastChar = Nothing,
      orgStateKeywords = [],
      orgStateLinkFormatters = mempty,
      orgStateTodoSequences = []
    }

-- | Map of functions for link transformations.  The map key is refers to the
-- link-type, the corresponding function transforms the given link string.
type OrgLinkFormatters = Map Text (Text -> Text)

-- | Macro expander function
type MacroExpander = [Text] -> Text

-- | Collection of todo markers in the order in which items should progress
type TodoSequence = [TodoKeyword]

registerTodoSequence :: TodoSequence -> OrgParserState -> OrgParserState
registerTodoSequence todoSeq st =
  let curSeqs = orgStateTodoSequences st
   in st {orgStateTodoSequences = todoSeq : curSeqs}

-- | Get the current todo/done sequences. If no custom todo sequences have been
-- defined, return a list containing just the default todo/done sequence.
activeTodoSequences :: OrgParserState -> [TodoSequence]
activeTodoSequences st =
  let curSeqs = orgStateTodoSequences st
   in if null curSeqs
        then [[TodoKeyword Todo "TODO", TodoKeyword Done "DONE"]]
        else curSeqs

activeTodoMarkers :: OrgParserState -> TodoSequence
activeTodoMarkers = concat . activeTodoSequences
