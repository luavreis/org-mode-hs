-- |

module Org.Parser.Common where

import Prelude hiding (many, State)
import Org.Parser.Definitions
import Data.Char (digitToInt)
import qualified Data.Text as T

digitIntChar :: MonadParser m => m Int
digitIntChar = digitToInt <$> digitChar

integer :: MonadParser m => m Int
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

isAlphaAZ :: Char -> Bool
isAlphaAZ c = isLowerAZ c || isUpperAZ c

uppercaseAZ :: MonadParser m => m Char
uppercaseAZ = satisfy isUpperAZ
              <?> "uppercase A-Z character"

lowercaseAZ :: MonadParser m => m Char
lowercaseAZ = satisfy isLowerAZ
              <?> "lowercase a-z character"

alphaAZ :: MonadParser m => m Char
alphaAZ = satisfy isAlphaAZ
              <?> "a-z or A-Z character"

manyAlphaAZ :: MonadParser m => m Text
manyAlphaAZ = takeWhileP (Just "a-z or A-Z characters")
              isAlphaAZ

isSpaceOrTab :: Char -> Bool
isSpaceOrTab c = c == ' ' || c == '\t'

spaceOrTab :: MonadParser m => m Char
spaceOrTab = satisfy isSpaceOrTab <?> "space or tab character"

-- | Skips one or more spaces or tabs.
skipSpaces1 :: MonadParser m => m ()
skipSpaces1 = void $ takeWhile1P (Just "at least one space or tab whitespace") isSpaceOrTab

-- | Skips zero or more spaces or tabs.
skipSpaces :: MonadParser m => m ()
skipSpaces = void $ takeWhileP (Just "space or tab whitespace") isSpaceOrTab

-- | Makes sure a value is Just, else fail with a custom
-- error message.
guardMaybe :: (MonadFail m, MonadParser m) => String -> Maybe a -> m a
guardMaybe _ (Just x) = pure x
guardMaybe err _      = fail err

-- | Parse the rest of line, returning the contents without the final newline.
anyLine :: MonadParser m => m (Tokens Text)
anyLine = takeWhileP (Just "rest of line") (/= '\n')
          <* (eof <|> void newline)

-- | Parse a line with whitespace contents.
blankline :: MonadParser m => m ()
blankline = try $ hspace <* newline

findChars2 :: MonadParser m => Char -> Char -> Maybe String -> m Text
findChars2 needle post descr =
  fix $ \search -> do
    partial <- takeWhileP descr (/= needle)
    _ <- char needle
    char post $> partial
      <|> (partial `T.snoc` needle <>) <$> search

parseFromText :: MonadParser m => Maybe (State Text Void) -> Text -> m b -> m b
parseFromText mState txt parser = do
  currentState <- getParserState
  let previousState = fromMaybe currentState mState
  setParserState previousState { stateInput = txt }
  result <- parser
  afterState <- getParserState
  setParserState currentState
    { stateParseErrors = stateParseErrors currentState
                         ++ stateParseErrors afterState }
  pure result

