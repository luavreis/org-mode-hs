-- |

module Text.Org.Parser.OrgElements where
import Text.Org.Parser.Definitions
import Text.Org.Parser.ElementStarts
import Text.Org.Parser.OrgObjects
import Text.Org.Parser.MarkupContexts
import qualified Text.Org.Builder as B

para :: OrgParser (F OrgElements)
para = do
  inls <- runMContext
          (mark " \t\n\r" $ try $ hspace *> eof <|> (newline *> endOfBlock)) -- Make this better
          (plainMarkupContext standardP)
  pure $ B.para <$> inls
