-- |

module Text.Org.Parser.OrgElements where
import Text.Org.Parser.Definitions
import Text.Org.Parser.ElementStarts
import Text.Org.Parser.OrgObjects
import qualified Text.Org.Builder as B

para :: Monad m => OrgParser m (F OrgElements)
para = do
  inls <- runMContext
          (mark " \t\n\r" $ try $ hspace *> eof <|> (newline *> endOfBlock)) -- Make this better
          (plainMarkupContext standardP)
  pure $ B.para <$> inls
