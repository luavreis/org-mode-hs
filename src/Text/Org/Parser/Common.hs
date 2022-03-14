{-# LANGUAGE TypeFamilies #-}
-- |

module Text.Org.Parser.Common where
import Prelude hiding (many)
import Text.Org.Parser.Definitions
import Data.Char (digitToInt)

digitIntChar :: OrgParser m Int
digitIntChar = digitToInt <$> digitChar

integer :: OrgParser m Int
integer = try $ do
  digits <- reverse <$> many digitIntChar
  let toInt (x:xs) = 10 * toInt xs + x
      toInt [] = 0
  pure $ toInt digits

number
  :: Int
  -> OrgParser m Int
number 1 = digitIntChar
number n | n > 1 = try $ do
             d <- digitIntChar
             (10 ^ (n - 1) * d +) <$> number (n - 1)
number _ = error "Number of digits to parse must be positive!"

uppercaseAZ :: OrgParser m Char
uppercaseAZ = satisfy (\c -> 'A' <= c && c <= 'Z')
              <?> "uppercase letter character"

isSpaceOrTab :: Char -> Bool
isSpaceOrTab c = c == ' ' || c == '\t'

spaceOrTab :: OrgParser m Char
spaceOrTab = satisfy isSpaceOrTab <?> "space or tab character"

-- | Skips zero or more spaces or tabs.
skipSpaces :: OrgParser m ()
skipSpaces = void $ takeWhileP (Just "space or tab whitespace") isSpaceOrTab

-- | Makes sure a value is Just, else fail with a custom
-- error message.
guardMaybe :: String -> Maybe a -> OrgParser m a
guardMaybe _ (Just x) = pure x
guardMaybe err _      = fail err

-- | Parse any line of text, returning the contents without the
-- final newline.
anyLine :: OrgParser m (Tokens Text)
anyLine = takeWhileP (Just "rest of line") (/= '\n')
          <* void anySingle

-- | Parse a line with whitespace contents.
blankline :: OrgParser m ()
blankline = try $ hspace <* newline
