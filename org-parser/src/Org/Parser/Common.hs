module Org.Parser.Common where

import Data.Char (digitToInt, isAsciiLower, isAsciiUpper)
import Data.Text qualified as T
import Org.Parser.Definitions
import Prelude hiding (State, many, some)

-- | The same as 'string'', but cheaper (?)
string'' :: MonadParser m => Text -> m Text
string'' = tokens ((==) `on` T.toLower)
{-# INLINE string'' #-}

digitIntChar :: MonadParser m => m Int
digitIntChar = digitToInt <$> digitChar

digits :: MonadParser m => m Text
digits = takeWhileP (Just "digits") isDigit

digits1 :: MonadParser m => m Text
digits1 = takeWhile1P (Just "digits") isDigit

integer :: MonadParser m => m Int
integer = try $ do
  digits' <- reverse <$> some digitIntChar
  let toInt (x : xs) = 10 * toInt xs + x
      toInt [] = 0
  pure $ toInt digits'

number ::
  Int ->
  OrgParser Int
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
upperAscii =
  satisfy isAsciiUpper
    <?> "uppercase A-Z character"

lowerAscii :: MonadParser m => m Char
lowerAscii =
  satisfy isAsciiLower
    <?> "lowercase a-z character"

asciiAlpha :: MonadParser m => m Char
asciiAlpha =
  satisfy isAsciiAlpha
    <?> "a-z or A-Z character"

manyAsciiAlpha :: MonadParser m => m Text
manyAsciiAlpha =
  takeWhileP
    (Just "a-z or A-Z characters")
    isAsciiAlpha

someAsciiAlpha :: MonadParser m => m Text
someAsciiAlpha =
  takeWhile1P
    (Just "a-z or A-Z characters")
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
guardMaybe err _ = fail err

-- | Parse a newline or EOF. Consumes no input at EOF!
newline' :: MonadParser m => m ()
newline' = void newline <|> eof

-- | Parse the rest of line, returning the contents without the final newline.
anyLine :: MonadParser m => m (Tokens Text)
anyLine =
  takeWhileP (Just "rest of line") (/= '\n')
    <* newline
{-# INLINE anyLine #-}

-- | Parse the rest of line, returning the contents without the final newline or EOF.
-- Consumes no input at EOF!
anyLine' :: MonadParser m => m (Tokens Text)
anyLine' =
  takeWhileP (Just "rest of line") (/= '\n')
    <* newline'

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

parseFromText :: FullState -> Text -> OrgParser b -> OrgParser b
parseFromText (prevPS, prevOS) txt parser = do
  (cPS, cOS) <- getFullState
  setFullState
    ( prevPS {stateInput = txt},
      -- NOTE: using cOS instead of prevOS
      -- is an implementation quirk. We
      -- don't want neither the changes of
      -- state done by the end parser in
      -- markupContext nor the ones in the
      -- fromText parser to be lost. But
      -- this will have the effect of
      -- commuting the change of state: the
      -- end changes will be registered
      -- before the body ones. This is not a
      -- problem because most of state
      -- building is commutative and most
      -- querying is done in Future anyway.
      -- The problematic ones are either
      -- irrelevant to a paragraph (like the
      -- order in which title keywords are
      -- concatenated) or must be handled
      -- manually like affiliated keywords.
      cOS
        { orgStateLastChar = orgStateLastChar prevOS,
          orgStatePendingAffiliated = orgStatePendingAffiliated prevOS
        }
    )
  result <- parser
  (aPS, aOS) <- getFullState
  setFullState
    ( cPS
        { stateParseErrors =
            stateParseErrors cPS
              ++ stateParseErrors aPS
        },
      aOS
        { orgStateLastChar =
            orgStateLastChar cOS,
          orgStatePendingAffiliated =
            orgStatePendingAffiliated cOS
        }
    )
  pure result
