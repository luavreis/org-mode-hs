-- |

module Text.Org.Parser.Definitions
  ( module Text.Org.Parser.Definitions
  , module Text.Org.Types
  , module Text.Org.Builder
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Debug
  , module Text.Org.Parser.State
  , module Data.Char
  ) where
import Text.Org.Types
import Text.Org.Builder (OrgElements, OrgInlines)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.Org.Parser.State
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)

type Parser = ParsecT Void Text Identity

type OrgParser = StateT OrgParserState Parser

getState :: OrgParser OrgParserState
getState = get

updateState
  :: (OrgParserState -> OrgParserState)
  -> OrgParser ()
updateState = modify

pureF :: Monad m => a -> m (F a)
pureF = pure . pure

defaultState :: OrgParserState
defaultState =  OrgParserState
  { orgStateAnchorIds            = []
  , orgStateLastChar             = Nothing
  , orgStateExcludeTags          = mempty
  , orgStateExcludeTagsChanged   = False
  , orgStateIdentifiers          = mempty
  , orgStateKeywords             = []
  , orgStateLinkFormatters       = mempty
  , orgStateMacros               = mempty
  , orgStateMacroDepth           = 1
  , orgStateNotes'               = []
  , orgStateTodoSequences        = []
  , orgStateTrimLeadBlkIndent    = False
  }

type InlineParser m = OrgParser (F OrgInlines)

data Marked m a = Marked
  { getMarks :: Set Char -- TODO use (Pred {toPred :: Char -> Bool})
  , getParser :: m a
  }

mapParser :: (m1 a -> m2 a) -> Marked m1 a -> Marked m2 a
mapParser f (Marked marks parser) = Marked marks (f parser)

mark :: String -> m a -> Marked m a
mark = Marked . fromList

instance Functor m => Functor (Marked m) where
  fmap f x@(Marked _ p) = x { getParser = fmap f p }

instance Alternative m => Semigroup (Marked m a) where
  Marked s1 p1 <> Marked s2 p2 =
    Marked (s1 <> s2) (p1 <|> p2)

instance Alternative m => Monoid (Marked m a) where
  mempty = Marked mempty empty
  mconcat xs = Marked (foldMap getMarks xs) (choice $ map getParser xs)

type MOrgParser m = Marked OrgParser
