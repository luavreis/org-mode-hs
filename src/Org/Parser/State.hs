-- |

module Org.Parser.State where

import Org.Types
import Org.Builder (OrgElements, OrgInlines)

type F = Ap (Reader OrgParserState)

data OrgOptions = OrgOptions
  { orgSrcPreserveIndentation :: Bool
  , orgTabWidth :: Int
  }

defaultOrgOptions :: OrgOptions
defaultOrgOptions = OrgOptions
  { orgSrcPreserveIndentation = False
  , orgTabWidth               = 4
  }

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStateInternalTargets      :: Map Text (Id, F OrgInlines) -- ^ Key is target name and value is (type, default alias)
  , orgStatePendingAffiliated    :: Affiliated
  , orgStateOptions              :: OrgOptions
  , orgStateIdStack              :: [Id]
  , orgStateLastChar             :: Maybe Char
  , orgStateExcludeTags          :: Set Tag
  , orgStateExcludeTagsChanged   :: Bool
  , orgStateKeywords             :: [F (KeywordKey, KeywordValue)]
  , orgStateLinkFormatters       :: OrgLinkFormatters
  , orgStateMacros               :: Map Text MacroExpander
  , orgStateSrcLineNumber        :: Int
  , orgStateNotes'               :: OrgNoteTable
  , orgStateTodoSequences        :: [TodoSequence]
  , orgStateFootnotes            :: Map Text (F OrgElements)
  }

defaultState :: OrgParserState
defaultState =  OrgParserState
  { orgStateInternalTargets      = mempty
  , orgStatePendingAffiliated    = mempty
  , orgStateOptions              = defaultOrgOptions
  , orgStateIdStack              = fmap show [0::Int ..]
  , orgStateLastChar             = Nothing
  , orgStateExcludeTags          = mempty
  , orgStateExcludeTagsChanged   = False
  , orgStateKeywords             = []
  , orgStateLinkFormatters       = mempty
  , orgStateMacros               = mempty
  , orgStateSrcLineNumber        = 1
  , orgStateNotes'               = []
  , orgStateTodoSequences        = []
  , orgStateFootnotes            = mempty
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
  in st{ orgStateTodoSequences = todoSeq : curSeqs }

-- | Get the current todo/done sequences. If no custom todo sequences have been
-- defined, return a list containing just the default todo/done sequence.
activeTodoSequences :: OrgParserState -> [TodoSequence]
activeTodoSequences st =
  let curSeqs = orgStateTodoSequences st
  in if null curSeqs
     then [[ TodoKeyword Todo "TODO" , TodoKeyword Done "DONE" ]]
     else curSeqs

activeTodoMarkers :: OrgParserState -> TodoSequence
activeTodoMarkers = concat . activeTodoSequences
