-- |

module Text.Org.Parser.ElementStarts where
import Text.Org.Parser.Definitions
import Text.Org.Parser.Common
import qualified Data.Text as T

-- | Read the start of a header line, return the header level
headingStart :: OrgParser m Int
headingStart = try $
  (T.length <$> takeWhile1P (Just "heading bullets") (== '*'))
  <* char ' '
  <* skipSpaces

commentLineStart :: OrgParser m ()
commentLineStart = try $
  -- the first char after '#' must be a plain space character or a newline
  skipSpaces <* char '#' <* oneOf (" \n" :: String)

-- | Succeeds if the parser is at the end of a block.
endOfBlock :: OrgParser m ()
endOfBlock = lookAhead . try $
  blankline <|> anyBlockStart <|> eof
 where
   -- Succeeds if there is a new block starting at this position.
   anyBlockStart :: OrgParser m ()
   anyBlockStart = try . choice $
     [ commentLineStart
     ]
