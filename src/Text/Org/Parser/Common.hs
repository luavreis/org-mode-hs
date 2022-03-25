-- |

module Text.Org.Parser.Common where
import Prelude hiding (many, State)
import Text.Org.Parser.Definitions
import Data.Char (digitToInt)

digitIntChar :: OrgParser Int
digitIntChar = digitToInt <$> digitChar

integer :: OrgParser Int
integer = try $ do
  digits <- reverse <$> many digitIntChar
  let toInt (x:xs) = 10 * toInt xs + x
      toInt [] = 0
  pure $ toInt digits

number
  :: Int
  -> OrgParser Int
number 1 = digitIntChar
number n | n > 1 = try $ do
             d <- digitIntChar
             (10 ^ (n - 1) * d +) <$> number (n - 1)
number _ = error "Number of digits to parse must be positive!"

-- * ASCII alphabet character classes

isUpperAZ :: Char -> Bool
isUpperAZ c = 'A' <= c && c <= 'Z'

isLowerAZ :: Char -> Bool
isLowerAZ c = 'a' <= c && c <= 'z'

uppercaseAZ :: OrgParser Char
uppercaseAZ = satisfy isUpperAZ
              <?> "uppercase A-Z character"

manyAlphaAZ :: OrgParser Text
manyAlphaAZ = takeWhileP (Just "a-z or A-Z characters")
            (\c -> isLowerAZ c || isUpperAZ c)

isSpaceOrTab :: Char -> Bool
isSpaceOrTab c = c == ' ' || c == '\t'

spaceOrTab :: OrgParser Char
spaceOrTab = satisfy isSpaceOrTab <?> "space or tab character"

-- | Skips one or more spaces or tabs.
skipSpaces1 :: OrgParser ()
skipSpaces1 = void $ takeWhile1P (Just "at least one space or tab whitespace") isSpaceOrTab

-- | Skips zero or more spaces or tabs.
skipSpaces :: OrgParser ()
skipSpaces = void $ takeWhileP (Just "space or tab whitespace") isSpaceOrTab

-- | Makes sure a value is Just, else fail with a custom
-- error message.
guardMaybe :: String -> Maybe a -> OrgParser a
guardMaybe _ (Just x) = pure x
guardMaybe err _      = fail err

-- | Parse any line of text, returning the contents without the
-- final newline.
anyLine :: OrgParser (Tokens Text)
anyLine = takeWhileP (Just "rest of line") (/= '\n')
          <* void anySingle

-- | Parse a line with whitespace contents.
blankline :: OrgParser ()
blankline = try $ hspace <* newline

parseFromText :: MonadParsec e s m => Maybe (State s e) -> m b -> s -> m b
parseFromText mState parser txt = do
  currentState <- getParserState
  let previousState = fromMaybe currentState mState
  setParserState previousState { stateInput = txt }
  result <- parser
  afterState <- getParserState
  setParserState currentState
    { stateParseErrors = stateParseErrors currentState
                         ++ stateParseErrors afterState }
  pure result

