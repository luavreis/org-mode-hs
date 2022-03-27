-- |

module Org.Parser.ElementStarts where

import Org.Parser.Definitions
import Org.Parser.Common
import qualified Data.Text as T

-- TL;DR: blockstars are bad. Remove.

-- | Read the start of a header line, return the header level
headingStart :: OrgParser Int
headingStart = try $
  (T.length <$> takeWhile1P (Just "heading bullets") (== '*'))
  <* char ' '
  <* skipSpaces

commentLineStart :: OrgParser ()
commentLineStart = try $
  -- the first char after '#' must be a plain space character or a newline
  hspace <* char '#' <* oneOf (" \n" :: String)

-- | Succeeds if the parser is at the end of an element.
endOfElement :: OrgParser ()
endOfElement = lookAhead . try $
  blankline <|> anyElementStart <|> eof
 where
   anyElementStart :: OrgParser ()
   anyElementStart = try . choice $
     [ commentLineStart
     , void headingStart
     ]
