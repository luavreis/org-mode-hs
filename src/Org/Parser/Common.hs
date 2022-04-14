-- |

module Org.Parser.Common where

import Prelude hiding (many, some, State)
import Org.Parser.Definitions
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper)
import Relude.Extra (notMember)
import qualified Data.Text as T

digitIntChar :: MonadParser m => m Int
digitIntChar = digitToInt <$> digitChar

digits :: MonadParser m => m Text
digits = takeWhile1P (Just "digits") isDigit

integer :: MonadParser m => m Int
integer = try $ do
  digits' <- reverse <$> some digitIntChar
  let toInt (x:xs) = 10 * toInt xs + x
      toInt [] = 0
  pure $ toInt digits'

number
  :: Int
  -> OrgParser Int
number 1 = digitIntChar
number n | n > 1 = try $ do
             d <- digitIntChar
             (10 ^ (n - 1) * d +) <$> number (n - 1)
number _ = error "Number of digits to parse must be positive!"


-- * ASCII alphabet character classes

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

upperAscii' :: MonadParser m => m Int
upperAscii' = do
  c <- upperAscii
  pure $ ord c - ord 'A' + 1

lowerAscii' :: MonadParser m => m Int
lowerAscii' = do
  c <- lowerAscii
  pure $ ord c - ord 'a' + 1

asciiAlpha' :: MonadParser m => m Int
asciiAlpha' = lowerAscii' <|> upperAscii'

upperAscii :: MonadParser m => m Char
upperAscii = satisfy isAsciiUpper
             <?> "uppercase A-Z character"

lowerAscii :: MonadParser m => m Char
lowerAscii = satisfy isAsciiLower
             <?> "lowercase a-z character"

asciiAlpha :: MonadParser m => m Char
asciiAlpha = satisfy isAsciiAlpha
              <?> "a-z or A-Z character"

manyAsciiAlpha :: MonadParser m => m Text
manyAsciiAlpha = takeWhileP (Just "a-z or A-Z characters")
                 isAsciiAlpha

someAsciiAlpha :: MonadParser m => m Text
someAsciiAlpha = takeWhile1P (Just "a-z or A-Z characters")
                 isAsciiAlpha

someNonSpace :: MonadParser m => m Text
someNonSpace = takeWhile1P (Just "not whitespace") (not . isSpace)

isSpaceOrTab :: Char -> Bool
isSpaceOrTab c = c == ' ' || c == '\t'

spaceOrTab :: MonadParser m => m Char
spaceOrTab = satisfy isSpaceOrTab <?> "space or tab character"

spacesOrTabs :: (MonadParser m, MonadState OrgParserState m) => m Int
spacesOrTabs = try $ sum <$> many (oneSpace <|> oneTab)
  where
    oneSpace = char ' ' $> 1
    oneTab = char '\t' >> getsO orgTabWidth

spacesOrTabs1 :: (MonadParser m, MonadState OrgParserState m) => m Int
spacesOrTabs1 = sum <$> some (oneSpace <|> oneTab)
  where
    oneSpace = char ' ' $> 1
    oneTab = char '\t' >> getsO orgTabWidth

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

-- | Parse a newline or EOF. Consumes no input at EOF!
newline' :: MonadParser m => m ()
newline' = void newline <|> eof

-- | Parse the rest of line, returning the contents without the final newline.
-- Consumes no input at EOF!
anyLine :: MonadParser m => m (Tokens Text)
anyLine = takeWhileP (Just "rest of line") (/= '\n')
          <* newline

-- | Consumes the rest of input
takeInput :: MonadParser m => m Text
takeInput = takeWhileP Nothing (const True)

-- | Parse a line with whitespace contents, and consume a newline at the end.
blankline :: MonadParser m => m ()
blankline = try $ hspace <* newline

-- | Parse a line with whitespace contents, line may end with EOF. CAUTION: this
-- function may consume NO INPUT! Be mindful of infinite loops!
blankline' :: MonadParser m => m ()
blankline' = try $ hspace <* newline'

findMarked :: forall b.
  Marked OrgParser b
  -> OrgParser (Text, b)
findMarked end = try $
  fix $ \search -> do
    str <- takeWhileP
           (Just $ "insides of mcontext (chars outside  " ++
            show (toList marks) ++ ")")
           (`notMember` marks)
    setLastChar (snd <$> T.unsnoc str)
    ((str,) <$> try (getParser end) <?> "end of mcontext")
      <|> (do c <- anySingle <?> "insides of mcontext (single)"
              first (T.snoc str c <>) <$> search)
  where
    marks = getMarks end
-- {-# INLINE findMarked #-}

findChars2 :: MonadParser m => Char -> Char -> Maybe String -> m Text
findChars2 needle post descr = try $
  fix $ \search -> do
    partial <- takeWhileP descr (/= needle)
    _ <- char needle
    char post $> partial
      <|> (partial `T.snoc` needle <>) <$> search

parseFromText :: FullState -> Text -> OrgParser b -> OrgParser b
parseFromText (prevPS, prevOS) txt parser = do
  (cPS, cOS) <- getFullState
  setFullState ( prevPS { stateInput = txt }
               , cOS { orgStateLastChar =
                       orgStateLastChar prevOS } )
  result <- parser
  (aPS, aOS) <- getFullState
  setFullState ( cPS { stateParseErrors =
                       stateParseErrors cPS ++
                       stateParseErrors aPS }
               , aOS { orgStateLastChar =
                       orgStateLastChar cOS } )
  pure result
