-- |

module Text.Org.Parser.Definitions
  ( module Text.Org.Parser.Definitions
  , module Text.Org.Types
  , module Text.Org.Builder
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Org.Parser.State
  , module Data.Char
  ) where
import Text.Org.Types
import Text.Org.Builder (OrgElements, OrgInlines)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Org.Parser.State
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)

type OrgParser m = StateT OrgParserState (ParsecT Void Text m)

getState :: OrgParser m OrgParserState
getState = get

updateState
  :: (OrgParserState -> OrgParserState)
  -> OrgParser m ()
updateState = modify

pureF :: a -> OrgParser m (F a)
pureF = pure . pure
