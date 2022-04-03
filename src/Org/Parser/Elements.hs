-- |

module Org.Parser.Elements where

import Prelude hiding (many, some)
import Org.Parser.Definitions
import Org.Parser.Common
import Org.Parser.Objects
import Org.Parser.MarkupContexts
import Relude.Extra hiding (next)
import qualified Org.Builder as B
import qualified Data.Text as T

-- | Read the start of a header line, return the header level
headingStart :: OrgParser Int
headingStart = try $
  (T.length <$> takeWhile1P (Just "heading bullets") (== '*'))
  <* char ' '
  <* skipSpaces

commentLine :: OrgParser (F OrgElements)
commentLine = try do
  hspace
  _ <- char '#'
  _ <- newline $> ""
       <|> char ' ' *> anyLine
  pure mempty

elements :: OrgParser (F OrgElements)
elements = mconcat <$> manyTill (element <|> para) (void (lookAhead headingStart) <|> eof)

element :: OrgParser (F OrgElements)
element = choice [ blankline $> mempty
                 , commentLine
                 , exampleBlock
                 , srcBlock
                 , exportBlock
                 , plainList
                 ] <?> "org element"

para :: OrgParser (F OrgElements)
para = try do
  (inls, next) <- withMContext_
                  (mark "\n" end)
                  (plainMarkupContext standardSet)
  pure $ (B.para <$> inls) <> next
  where
    end :: OrgParser (F OrgElements)
    end = eof $> mempty <|> try do
      _ <- newline
      lookAhead headingStart $> mempty
        <|> element

-- * Plain lists

plainList :: OrgParser (F OrgElements)
plainList = do
  (indent, fstItem) <- listItem
  rest <- many . try $ guardIndent indent >> snd <$> listItem
  let kind = listItemType <$> fstItem
      items = (:) <$> fstItem <*> sequence rest
  withAffiliated \aff -> B.list aff <$> kind <*> items
  where
    guardIndent i = guard . (== i) =<< spacesOrTabs

listItem :: OrgParser (Int, F ListItem)
listItem = try do
  (indent, bullet) <- unorderedBullet <|> counterBullet
  hspace1 <|> lookAhead (void newline)
  cookie <- optional counterSet
  box    <- optional checkbox
  tag    <- case bullet of
    Bullet _ -> toList <<$>> option mempty listItemTag
    _        -> pureF []
  els <- liftA2 (<>) (blankline $> mempty <|> indentedPara indent)
                     (indentedElements indent)
  pure (indent, ListItem bullet cookie box <$> tag <*> (toList <$> els))
  where
    unorderedBullet = fmap (second Bullet) $
      try ((,) <$> spacesOrTabs  <*> satisfy \c -> c == '+' || c == '-') <|>
      try ((,) <$> spacesOrTabs1 <*> char '*')
    counterBullet = try do
      indent <- spacesOrTabs
      counter <- digits <|> T.singleton <$> satisfy isAsciiAlpha
      d   <- satisfy \c -> c == '.' || c == ')'
      pure (indent, Counter counter d)

counterSet :: OrgParser Int
counterSet = try $
  string "[@"
  *> parseNum
  <* char ']'
  <* hspace
  where
    parseNum = integer <|> asciiAlpha'

checkbox :: OrgParser Checkbox
checkbox = try $
  char '['
  *> tick
  <* char ']'
  <* hspace1
  where
    tick = char ' ' $> BoolBox False <|>
           char 'X' $> BoolBox True  <|>
           char '-' $> PartialBox

listItemTag :: OrgParser (F OrgInlines)
listItemTag = try do
  st <- getFullState
  (contents, found) <- findMarked end
  guard found
  parseFromText st contents (plainMarkupContext standardSet)
  where
    end = mark " \t\n" $
      spaceOrTab *> string "::" *> spaceOrTab $> True
        <|> newline $> False

indentedPara :: Int -> OrgParser (F OrgElements)
indentedPara indent = try do
  (inls, next) <- withMContext_
                  (mark "\n" end)
                  (plainMarkupContext standardSet)
  pure $ (B.para <$> inls) <> next
  where
    end :: OrgParser (F OrgElements)
    end = eof $> mempty <|> try do
      _ <- newline
      blankline' $> mempty
        <|> lookAhead headingStart $> mempty
        <|> try (guard . (<= indent) =<< spacesOrTabs) $> mempty
        <|> element

indentedElements :: Int -> OrgParser (F OrgElements)
indentedElements indent =
  mconcat <$> many indentedElement
  where
    indentedElement = try do
      notFollowedBy headingStart
      blankline $> mempty <|> do
        guard . (> indent) =<< lookAhead spacesOrTabs
        element <|> indentedPara indent


-- * Lesser blocks

exampleBlock :: OrgParser (F OrgElements)
exampleBlock = try do
  hspace
  _ <- string' "#+begin_example"
  switches <- blockSwitches
  _ <- anyLine
  startingNumber <- updateLineNumbers switches
  contents <- rawBlockContents end switches
  pure <$> (withAffiliated B.example ?? startingNumber ?? switches ?? contents)
  where
    end = try $ hspace *> string' "#+end_example" <* blankline'

srcBlock :: OrgParser (F OrgElements)
srcBlock = try do
  hspace
  _ <- string' "#+begin_src"
  lang <- option "" $ hspace1 *> someNonSpace
  (switches, args) <- liftA2 (,) blockSwitches headerArgs
  _ <- anyLine
  num <- updateLineNumbers switches
  contents <- rawBlockContents end switches
  pure <$> (withAffiliated B.srcBlock ?? lang ?? num ?? switches ?? args ?? contents)
  where
    end = try $ hspace *> string' "#+end_src" <* blankline'
    headerArg = liftA2 (,) (hspace1 *> char ':' *> someNonSpace)
                           (T.strip <$> takeWhileP Nothing (\c -> c /= ':' && c /= '\n'))
    headerArgs = fromList <$> many headerArg

exportBlock :: OrgParser (F OrgElements)
exportBlock = try do
  hspace
  _ <- string' "#+begin_export"
  format <- option "" $ hspace1 *> someNonSpace
  _ <- anyLine
  contents <- T.unlines <$> manyTill anyLine end
  pure <$> (withAffiliated B.export ?? format ?? contents)
  where
    end = try $ hspace *> string' "#+end_export" <* blankline'

indentContents :: Int -> [SrcLine] -> [SrcLine]
indentContents tabWidth (map (srcLineMap $ tabsToSpaces tabWidth) -> lins) =
  map (srcLineMap $ T.drop minIndent) lins
  where
    minIndent = minimum1 (0 :| map (indentSize . srcLineContent) lins)
    indentSize = T.length . T.takeWhile (== ' ')

tabsToSpaces :: Int -> Text -> Text
tabsToSpaces tabWidth txt =
  T.span (\c -> c == ' ' || c == '\t') txt
  & first (flip T.replicate " "
            . uncurry (+)
            . bimap T.length ((* tabWidth) . T.length)
            . T.partition (== ' '))
  & uncurry (<>)

updateLineNumbers :: Map Text Text -> OrgParser (Maybe Int)
updateLineNumbers switches =
  case "-n" `lookup` switches of
    Just (readMaybe . toString -> n) -> setSrcLineNum (fromMaybe 0 n)
                                     *> fmap Just getSrcLineNum
    _ -> case "+n" `lookup` switches of
           Just (readMaybe . toString -> n) -> incSrcLineNum (fromMaybe 0 n)
                                            *> fmap Just getSrcLineNum
           _ -> pure Nothing

rawBlockContents :: OrgParser void -> Map Text Text -> OrgParser [SrcLine]
rawBlockContents end switches = do
  contents <- manyTill (rawBlockLine switches) end
  tabWidth       <- getsO orgTabWidth
  preserveIndent <- getsO orgSrcPreserveIndentation
  pure $ if preserveIndent || "-i" `member` switches
         then map (srcLineMap (tabsToSpaces tabWidth)) contents
         else indentContents tabWidth contents

quotedLine :: OrgParser Text
quotedLine = do
  (<>) <$> option "" (try $ char ',' *> (string "*" <|> string "#+"))
       <*> anyLine

rawBlockLine :: Map Text Text -> OrgParser SrcLine
rawBlockLine switches = try $
  (applyRef =<< quotedLine)
    <* incSrcLineNum 1
  where
    (refpre, refpos) = maybe ("(ref:", ")")
                       (second (T.drop 2) . T.breakOn "%s") $
                       lookup "-l" switches
    applyRef txt
      | Just (ref, content) <- flip parseMaybe (T.reverse txt) do
          (hspace :: Parsec Void Text ())
          _ <- string (T.reverse refpos)
          ref <- toText . reverse <$> someTill
                 (satisfy $ \c -> isAsciiAlpha c || isDigit c || c == '-')
                 (string $ T.reverse refpre)
          content <- T.stripEnd . T.reverse <$> takeInput
          pure (ref, content)
        = do
          alias <- if "-r" `member` switches
                   then show <$> getSrcLineNum
                   else pure ref
          registerTarget ("(" <> ref <> ")") Coderef (pure $ B.plain alias)
          pure $ RefLine ref content
      | otherwise = pure $ SrcLine txt

blockSwitches :: OrgParser (Map Text Text)
blockSwitches = fromList <$> many (linum <|> switch <|> fmt)
  where
    linum :: OrgParser (Text, Text)
    linum = try $ do
      hspace1
      s <- T.snoc . one <$> oneOf ['+', '-']
                        <*> char 'n'
      num <- option "" $ hspace1 *> takeWhileP Nothing isDigit
      return (s, num)

    fmt :: OrgParser (Text, Text)
    fmt = try $ do
      hspace1
      s <- string "-l"
      hspace1
      str <- between (char '"') (char '"') $
             takeWhileP Nothing (\c -> c /= '"' && c /= '\n')
      return (s, str)

    switch :: OrgParser (Text, Text)
    switch = try $ do
      hspace1
      s <- T.snoc . one <$> char '-'
                        <*> oneOf ['i', 'k', 'r']
      pure (s, "")
