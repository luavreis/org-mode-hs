-- |

module Org.Parser.Elements where

import Prelude hiding (many)
import Org.Parser.Definitions
import Org.Parser.Common
import Org.Parser.ElementStarts
import Org.Parser.Objects
import Org.Parser.MarkupContexts
import Relude.Extra
import qualified Org.Builder as B
import qualified Data.Text as T

elements :: OrgParser (F OrgElements)
elements = mconcat <$> manyTill element (void (lookAhead headingStart) <|> eof)

element :: OrgParser (F OrgElements)
element = choice [ para
                 ] <?> "org element"

para :: OrgParser (F OrgElements)
para = do
  inls <- runMContext
          (mark " \t\n\r" $ try $ hspace *> eof <|> (newline *> endOfElement)) -- Make this better
          (plainMarkupContext standardSet)
  pure $ B.para <$> inls

exampleBlock :: OrgParser (F OrgElements)
exampleBlock = try $ do
  hspace
  _ <- string' "#+begin_example"
  switches <- newline $> mempty
              <|> hspace1 *> blockSwitches <* anyLine
  contents <- manyTill (rawBlockLine switches) end
  pure <$> (withAffiliated B.example ?? switches ?? contents)
  where
    end = try $ hspace *> string' "#+end_example" *> hspace *> newline

rawBlockLine :: Map Text Text -> OrgParser SrcLine
rawBlockLine switches = try $ do
  notFollowedBy headingStart
  quoted <- option "" (try $ char ',' *> (string "*" <|> string "#+"))
  applyRef . (quoted <>) =<< anyLine
    <* incrementLineNum
  where
    refFormat = second (T.drop 2) . T.breakOn "%s" <$> lookup "-l" switches
    applyRef txt = case refFormat of
       Just (refpre, refpos)
         | Just (ref, content) <- flip parseMaybe (T.reverse txt) $ do
             (hspace :: Parsec Void Text ())
             _ <- string (T.reverse refpos)
             ref <- toText <$> someTill
                    (satisfy $ \c -> isAlphaAZ c || isDigit c || c == '-')
                    (string $ T.reverse refpre)
             content <- T.reverse <$> getInput
             let content' =
                   if "-r" `member` switches && "-k" `notMember` switches
                   then T.stripEnd content
                   else T.stripEnd content <> " (" <> ref <> ")"
             pure (ref, content')
           -> do
             alias <- if "-r" `member` switches
                      then show <$> lineNum
                      else pure ref
             registerTarget ref Coderef $ pure $ B.plain alias
             pure $ RefLine ref content
       _ -> pure $ SrcLine txt
    lineNum = gets orgStateSrcLineNumber
    incrementLineNum = updateState $ \s ->
      s { orgStateSrcLineNumber = orgStateSrcLineNumber s + 1 }

blockSwitches :: OrgParser (Map Text Text)
blockSwitches = fromList <$> many (linum <|> switch <|> fmt)
  where
    linum :: OrgParser (Text, Text)
    linum = try $ do
      s <- T.snoc . one <$> oneOf ['+', '-']
                        <*> char 'n'
      hspace1
      num <- takeWhileP Nothing isDigit <* hspace1
             <|> pure ""
      return (s, num)

    fmt :: OrgParser (Text, Text)
    fmt = try $ do
      s <- string "-l"
      hspace1
      str <- between (char '"') (char '"') $
             takeWhileP Nothing (\c -> c /= '"' && c /= '\n')
      hspace1
      return (s, str)

    switch :: OrgParser (Text, Text)
    switch = try $ do
      s <- T.snoc . one <$> char '-'
                        <*> oneOf ['i', 'k', 'r']
      hspace1
      pure (s, "")
