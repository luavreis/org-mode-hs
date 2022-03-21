-- |

module Text.Org.Parser.State where
import Text.Org.Types
import Text.Org.Builder (OrgElements)

type F = Ap (Reader OrgParserState)

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStateAnchorIds            :: [Text]
  , orgStateLastChar             :: Maybe Char
  , orgStateExcludeTags          :: Set Tag
  , orgStateExcludeTagsChanged   :: Bool
  , orgStateIdentifiers          :: Set Text
  , orgStateKeywords             :: [F KeywordPair]
  , orgStateLinkFormatters       :: OrgLinkFormatters
  , orgStateMacros               :: Map Text MacroExpander
  , orgStateMacroDepth           :: Int
  , orgStateNotes'               :: OrgNoteTable
  , orgStateTodoSequences        :: [TodoSequence]
  , orgStateTrimLeadBlkIndent    :: Bool
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
