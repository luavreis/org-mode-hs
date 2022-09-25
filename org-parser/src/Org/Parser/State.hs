module Org.Parser.State where

import Org.Builder (OrgElements, OrgObjects)
import Org.Types

type F = Ap (Reader OrgParserState)

data OrgOptions = OrgOptions
  { orgSrcPreserveIndentation :: Bool,
    orgTabWidth :: Int,
    orgElementParsedKeywords :: [Text],
    orgElementDualKeywords :: [Text],
    orgElementAffiliatedKeywords :: [Text]
  }

defaultOrgOptions :: OrgOptions
defaultOrgOptions =
  OrgOptions
    { orgSrcPreserveIndentation = False,
      orgTabWidth = 4,
      orgElementParsedKeywords = ["caption", "title", "subtitle", "date", "author"],
      orgElementDualKeywords = ["caption", "results"],
      orgElementAffiliatedKeywords = ["caption", "data", "header", "headers", "label", "name", "plot", "resname", "result", "source", "srcname", "tblname"]
    }

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { -- | Key is target name and value is (anchor, default alias)
    orgStateInternalTargets :: Map Text (Id, F OrgObjects),
    -- | This is the set of known anchors, in order to avoid invalid documents.
    orgStateKnownAnchors :: Set Id,
    orgStatePendingAffiliated :: [F (KeywordKey, KeywordValue)],
    orgStateTargetDescriptionCtx :: Maybe (F OrgObjects),
    orgStateOptions :: OrgOptions,
    orgStateIdStack :: [Id],
    orgStateLastChar :: Maybe Char,
    orgStateExcludeTags :: Set Tag,
    orgStateExcludeTagsChanged :: Bool,
    orgStateKeywords :: [F (KeywordKey, KeywordValue)],
    orgStateLinkFormatters :: OrgLinkFormatters,
    orgStateMacros :: Map Text MacroExpander,
    orgStateSrcLineNumber :: Int,
    orgStateNotes' :: OrgNoteTable,
    orgStateTodoSequences :: [TodoSequence],
    orgStateFootnotes :: Map Text (F OrgElements)
  }

defaultState :: OrgParserState
defaultState =
  OrgParserState
    { orgStateInternalTargets = mempty,
      orgStateKnownAnchors = mempty,
      orgStatePendingAffiliated = mempty,
      orgStateTargetDescriptionCtx = Nothing,
      orgStateOptions = defaultOrgOptions,
      orgStateIdStack = fmap show [0 :: Int ..],
      orgStateLastChar = Nothing,
      orgStateExcludeTags = mempty,
      orgStateExcludeTagsChanged = False,
      orgStateKeywords = [],
      orgStateLinkFormatters = mempty,
      orgStateMacros = mempty,
      orgStateSrcLineNumber = 1,
      orgStateNotes' = [],
      orgStateTodoSequences = [],
      orgStateFootnotes = mempty
    }

-- | Map of functions for link transformations.  The map key is refers to the
-- link-type, the corresponding function transforms the given link string.
type OrgLinkFormatters = Map Text (Text -> Text)

-- | Macro expander function
type MacroExpander = [Text] -> Text

-- | An inline note / footnote containing the note key and its (inline) value.
type OrgNoteRecord = (Text, F OrgElements)

-- | Table of footnotes
type OrgNoteTable = [OrgNoteRecord]

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
