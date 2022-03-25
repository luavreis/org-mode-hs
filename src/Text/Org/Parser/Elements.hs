-- |

module Text.Org.Parser.Elements where
import Prelude hiding (many)
import Text.Org.Parser.Definitions
import Text.Org.Parser.ElementStarts
import Text.Org.Parser.Objects
import Text.Org.Parser.MarkupContexts
import qualified Text.Org.Builder as B

elements :: OrgParser (F OrgElements)
elements = mconcat <$> manyTill element (void (lookAhead headingStart) <|> eof)

element :: OrgParser (F OrgElements)
element = choice [ para
                 ] <?> "org element"

para :: OrgParser (F OrgElements)
para = do
  inls <- runMContext
          (mark " \t\n\r" $ try $ hspace *> eof <|> (newline *> endOfBlock)) -- Make this better
          (plainMarkupContext standardSet)
  pure $ B.para <$> inls
