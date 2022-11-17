module Org.Parser.Elements where

import Data.Text qualified as T
import Org.Builder qualified as B
import Org.Parser.Common
import Org.Parser.Definitions
import Org.Parser.MarkupContexts
import Org.Parser.Objects
import Relude.Extra hiding (elems, next)
import Replace.Megaparsec
import Prelude hiding (many, some)

-- | Read the start of a header line, return the header level
headingStart :: OrgParser Int
headingStart =
  try $
    (T.length <$> takeWhile1P (Just "heading bullets") (== '*'))
      <* char ' '
      <* skipSpaces

commentLine :: OrgParser OrgElements
commentLine = try do
  hspace
  _ <- char '#'
  _ <-
    blankline' $> ""
      <|> char ' ' *> anyLine'
  pure mempty

elements :: OrgParser OrgElements
elements = elements' (void (lookAhead headingStart) <|> eof)

elements' :: OrgParser end -> OrgParser OrgElements
elements' end = mconcat <$> manyTill (element <|> para) end

-- | Each element parser must consume till the start of a line or EOF.
-- This is necessary for correct counting of list indentations.
element :: OrgParser OrgElements
element =
  elementNonEmpty
    <|> blankline'
      $> mempty
      <* clearPendingAffiliated
    <?> "org element or blank line"

elementNonEmpty :: OrgParser OrgElements
elementNonEmpty =
  elementIndentable
    <|> footnoteDef
      <* clearPendingAffiliated

elementIndentable :: OrgParser OrgElements
elementIndentable =
  affKeyword
    <|> choice
      [ commentLine,
        exampleBlock,
        srcBlock,
        exportBlock,
        greaterBlock,
        plainList,
        latexEnvironment,
        drawer,
        keyword,
        horizontalRule,
        table
      ]
      <* clearPendingAffiliated
    <?> "org element"

para :: OrgParser OrgElements
para = try do
  hspace
  f <- withAffiliated B.para
  (inls, next, _) <-
    withMContext__
      (/= '\n')
      end
      (plainMarkupContext standardSet)
  return $ f inls <> next
  where
    end :: OrgParser OrgElements
    end = try do
      _ <- newline'
      lookAhead headingStart $> mempty
        <|> eof $> mempty
        <|> lookAhead blankline $> mempty
        <|> elementNonEmpty

-- * Plain lists

plainList :: OrgParser OrgElements
plainList = try do
  f <- withAffiliated B.list
  (indent, fstItem) <- listItem
  rest <- many . try $ guardIndent indent =<< listItem
  let kind = listItemType fstItem
      items = fstItem : rest
  return $ f kind items
  where
    guardIndent indent (i, l) = guard (indent == i) $> l

listItem :: OrgParser (Int, ListItem)
listItem = try do
  (indent, bullet) <- unorderedBullet <|> counterBullet
  hspace1 <|> lookAhead (void newline')
  cookie <- optional counterSet
  box <- optional checkbox
  tag <- case bullet of
    Bullet _ -> toList <$> option mempty itemTag
    _ -> return []
  els <-
    liftA2
      (<>)
      (blankline' $> mempty <|> indentedPara indent)
      (indentedElements indent)
  return (indent, ListItem bullet cookie box tag (toList els))
  where
    unorderedBullet =
      fmap (second Bullet) $
        try ((,) <$> spacesOrTabs <*> satisfy \c -> c == '+' || c == '-')
          <|> try ((,) <$> spacesOrTabs1 <*> char '*')
    counterBullet = try do
      indent <- spacesOrTabs
      counter <- digits1 <|> T.singleton <$> satisfy isAsciiAlpha
      d <- satisfy \c -> c == '.' || c == ')'
      pure (indent, Counter counter d)

counterSet :: OrgParser Int
counterSet =
  try $
    string "[@"
      *> parseNum
      <* char ']'
      <* hspace
  where
    parseNum = integer <|> asciiAlpha'

checkbox :: OrgParser Checkbox
checkbox =
  try $
    char '['
      *> tick
      <* char ']'
      <* (hspace1 <|> lookAhead (void newline'))
  where
    tick =
      char ' ' $> BoolBox False
        <|> char 'X' $> BoolBox True
        <|> char '-' $> PartialBox

itemTag :: OrgParser OrgObjects
itemTag = try do
  clearLastChar
  st <- getFullState
  (contents, found) <- findSkipping (not . isSpace) end
  guard found
  parseFromText st contents (plainMarkupContext standardSet)
  where
    end =
      try (space1 *> string "::" *> space1 $> True)
        <|> newline' $> False

indentedPara :: Int -> OrgParser OrgElements
indentedPara indent = try do
  hspace
  f <- withAffiliated B.para
  (inls, next, _) <-
    withMContext__
      (/= '\n')
      end
      (plainMarkupContext standardSet)
  return $ f inls <> next
  where
    end :: OrgParser OrgElements
    end = try do
      _ <- newline'
      lookAhead blankline' $> mempty
        <|> lookAhead headingStart $> mempty
        -- We don't want to consume any indentation, so we look ahead.
        <|> lookAhead (try $ guard . (<= indent) =<< spacesOrTabs) $> mempty
        <|> element

indentedElements :: Int -> OrgParser OrgElements
indentedElements indent =
  mconcat <$> many indentedElement
  where
    indentedElement = try do
      notFollowedBy headingStart
      blankline
        *> notFollowedBy blankline'
        *> clearPendingAffiliated
        $> mempty
        <|> do
          guard . (> indent) =<< lookAhead spacesOrTabs
          elementNonEmpty <|> indentedPara indent

-- * Lesser blocks

exampleBlock :: OrgParser OrgElements
exampleBlock = try do
  hspace
  f <- withAffiliated B.example
  _ <- string'' "#+begin_example"
  switches <- blockSwitches
  _ <- anyLine
  contents <- rawBlockContents end switches
  pure $ f switches contents
  where
    end = try $ hspace *> string'' "#+end_example" <* blankline'

srcBlock :: OrgParser OrgElements
srcBlock = try do
  hspace
  f <- withAffiliated B.srcBlock
  _ <- string'' "#+begin_src"
  lang <- option "" $ hspace1 *> someNonSpace
  switches <- blockSwitches
  args <- headerArgs
  contents <- rawBlockContents end switches
  pure $ f lang switches args contents
  where
    end = try $ hspace *> string'' "#+end_src" <* blankline'

headerArgs :: StateT OrgParserState Parser [(Text, Text)]
headerArgs = do
  hspace
  fromList
    <$> headerArg
    `sepBy` hspace1
    <* anyLine'
  where
    headerArg =
      liftA2
        (,)
        (char ':' *> someNonSpace)
        ( T.strip . fst
            <$> findSkipping
              (not . isSpace)
              ( try $
                  lookAhead
                    ( newline'
                        <|> hspace1 <* char ':'
                    )
              )
        )

exportBlock :: OrgParser OrgElements
exportBlock = try do
  hspace
  _ <- string'' "#+begin_export"
  format <- option "" $ hspace1 *> someNonSpace
  _ <- anyLine
  contents <- T.unlines <$> manyTill anyLine end
  return $ B.export format contents
  where
    end = try $ hspace *> string'' "#+end_export" <* blankline'

-- verseBlock :: OrgParser OrgElements
-- verseBlock = try do
--   hspace
--   _ <- string'' "#+begin_verse"
--   undefined
--   where
-- end = try $ hspace *> string'' "#+end_export" <* blankline'

indentContents :: Int -> [SrcLine] -> [SrcLine]
indentContents tabWidth (map (srcLineMap $ tabsToSpaces tabWidth) -> lins) =
  map (srcLineMap $ T.drop minIndent) lins
  where
    minIndent = maybe 0 minimum1 (nonEmpty $ map (indentSize . srcLineContent) lins)
    indentSize = T.length . T.takeWhile (== ' ')

tabsToSpaces :: Int -> Text -> Text
tabsToSpaces tabWidth txt =
  T.span (\c -> c == ' ' || c == '\t') txt
    & first
      ( flip T.replicate " "
          . uncurry (+)
          . bimap T.length ((* tabWidth) . T.length)
          . T.partition (== ' ')
      )
    & uncurry (<>)

rawBlockContents :: OrgParser void -> Map Text Text -> OrgParser [SrcLine]
rawBlockContents end switches = do
  contents <- manyTill (rawBlockLine switches) end
  tabWidth <- getsO orgSrcTabWidth
  preserveIndent <- getsO orgSrcPreserveIndentation
  pure $
    if preserveIndent || "-i" `member` switches
      then map (srcLineMap (tabsToSpaces tabWidth)) contents
      else indentContents tabWidth contents

quotedLine :: OrgParser Text
quotedLine = do
  (<>)
    <$> option "" (try $ char ',' *> (string "*" <|> string "#+"))
    <*> anyLine

rawBlockLine :: Map Text Text -> OrgParser SrcLine
rawBlockLine switches =
  try $ applyRef =<< quotedLine
  where
    (refpre, refpos) =
      maybe
        ("(ref:", ")")
        (second (T.drop 2) . T.breakOn "%s")
        $ lookup "-l" switches
    applyRef txt
      | Just (content, ref, _) <- breakCap refCookie txt =
          pure $ RefLine "" ref content
      | otherwise = pure $ SrcLine txt
    refCookie :: Parser Text
    refCookie = do
      space1 <* string refpre
      toText
        <$> someTill
          (satisfy $ \c -> isAsciiAlpha c || isDigit c || c == '-' || c == ' ')
          (string refpos)

blockSwitches :: OrgParser (Map Text Text)
blockSwitches = fromList <$> many (linum <|> switch <|> fmt)
  where
    linum :: OrgParser (Text, Text)
    linum = try $ do
      hspace1
      s <-
        T.snoc . one
          <$> oneOf ['+', '-']
          <*> char 'n'
      num <- option "" $ try $ hspace1 *> takeWhileP Nothing isDigit
      _ <- lookAhead spaceChar
      return (s, num)

    fmt :: OrgParser (Text, Text)
    fmt = try $ do
      hspace1
      s <- string "-l"
      hspace1
      str <-
        between (char '"') (char '"') $
          takeWhileP Nothing (\c -> c /= '"' && c /= '\n')
      _ <- lookAhead spaceChar
      return (s, str)

    switch :: OrgParser (Text, Text)
    switch = try $ do
      hspace1
      s <-
        T.snoc . one
          <$> char '-'
          <*> oneOf ['i', 'k', 'r']
      _ <- lookAhead spaceChar
      pure (s, "")

-- * Greater Blocks

greaterBlock :: OrgParser OrgElements
greaterBlock = try do
  f <- withAffiliated B.greaterBlock
  hspace
  _ <- string'' "#+begin_"
  bname <- someNonSpace <* anyLine
  els <- withContext anyLine (end bname) elements
  clearPendingAffiliated
  return $ f (blockType bname) els
  where
    blockType = \case
      (T.toLower -> "center") -> Center
      (T.toLower -> "quote") -> Quote
      other -> Special other
    end :: Text -> OrgParser Text
    end name = try $ hspace *> string'' "#+end_" *> string'' name <* blankline'

-- * Drawers

drawer :: OrgParser OrgElements
drawer = try do
  hspace
  _ <- char ':'
  dname <- takeWhile1P (Just "drawer name") (\c -> c /= ':' && c /= '\n')
  char ':' >> blankline
  els <- withContext blankline end elements
  return $ B.drawer dname els
  where
    end :: OrgParser ()
    end = try $ newline *> hspace <* string'' ":end:"

-- * LaTeX Environments

latexEnvironment :: OrgParser OrgElements
latexEnvironment = try do
  hspace
  _ <- string "\\begin{"
  ename <-
    takeWhile1P
      (Just "latex environment name")
      (\c -> isAsciiAlpha c || isDigit c || c == '*')
  _ <- char '}'
  (str, _) <- findSkipping (/= '\\') (end ename)
  f <- withAffiliated B.latexEnvironment
  return $ f ename $ "\\begin{" <> ename <> "}" <> str <> "\\end{" <> ename <> "}"
  where
    end :: Text -> OrgParser ()
    end name = try $ string ("\\end{" <> name <> "}") *> blankline'

-- * Keywords and affiliated keywords

affKeyword :: OrgParser OrgElements
affKeyword = try do
  hspace
  _ <- string "#+"
  try do
    (T.toLower -> name) <-
      liftA2
        (<>)
        (string'' "attr_")
        (takeWhile1P Nothing (\c -> not (isSpace c || c == ':')))
    _ <- char ':'
    args <- headerArgs
    registerAffiliated (name, B.attrKeyword args)
    pure mempty
    <|> try do
      affkws <- getsO orgElementAffiliatedKeywords
      name <- choice (fmap (\s -> string'' s $> s) affkws)
      isdualkw <- (name `elem`) <$> getsO orgElementDualKeywords
      isparsedkw <- (name `elem`) <$> getsO orgElementParsedKeywords
      value <-
        if isparsedkw
          then do
            optArg <- option mempty $ guard isdualkw *> optionalArgP
            _ <- char ':'
            hspace
            st <- getFullState
            line <- anyLine'
            value <- parseFromText st line (plainMarkupContext standardSet)
            pure $ B.parsedKeyword optArg value
          else do
            _ <- char ':'
            hspace
            B.valueKeyword . T.stripEnd <$> anyLine'
      registerAffiliated (name, value)
      pure mempty
  where
    optionalArgP =
      withBalancedContext '[' ']' (\c -> c /= '\n' && c /= ':') $
        plainMarkupContext standardSet

keyword :: OrgParser OrgElements
keyword = try do
  hspace
  _ <- string "#+"
  -- This is one of the places where it is convoluted to replicate org-element
  -- regexes: "#+abc:d:e :f" is a valid keyword of key "abc:d" and value "e :f".
  name <-
    T.toLower . fst <$> fix \me -> do
      res@(name, ended) <-
        findSkipping (\c -> c /= ':' && not (isSpace c)) $
          try $
            (newline' <|> void (satisfy isSpace)) $> False
              <|> char ':' *> notFollowedBy me $> True
      guard (not $ T.null name)
      guard ended <?> "keyword end"
      pure res
  hspace
  value <- T.stripEnd <$> anyLine'
  return $ B.keyword name value

-- * Footnote definitions

footnoteDef :: OrgParser OrgElements
footnoteDef = try do
  lbl <- start
  _ <- optional blankline'
  def <-
    elements' $
      lookAhead $
        void headingStart
          <|> try (blankline' *> blankline')
          <|> void (try start)
  return $ B.footnoteDef lbl def
  where
    start =
      string "[fn:"
        *> takeWhile1P
          (Just "footnote def label")
          (\c -> isAlphaNum c || c == '-' || c == '_')
        <* char ']'

-- * Horizontal Rules

horizontalRule :: OrgParser OrgElements
horizontalRule = try do
  hspace
  l <- T.length <$> takeWhile1P (Just "hrule dashes") (== '-')
  guard (l >= 5)
  blankline'
  return B.horizontalRule

-- * Tables

table :: OrgParser OrgElements
table = try do
  hspace
  f <- withAffiliated B.table
  _ <- lookAhead $ char '|'
  rows <- some tableRow
  pure (f rows)
  where
    tableRow :: OrgParser TableRow
    tableRow = ruleRow <|> columnPropRow <|> standardRow

    ruleRow = try $ RuleRow <$ (hspace >> string "|-" >> anyLine')

    columnPropRow = try do
      hspace
      _ <- char '|'
      ColumnPropsRow
        <$> some cell
        <* blankline'
      where
        cell = do
          hspace
          Just <$> cookie <|> Nothing <$ void (char '|')
        cookie = try do
          a <-
            string "<l" $> AlignLeft
              <|> string "<c" $> AlignCenter
              <|> string "<r" $> AlignRight
          _ <- digits
          _ <- char '>'
          hspace
          void (char '|') <|> lookAhead newline'
          pure a

    standardRow = try do
      hspace
      _ <- char '|'
      B.standardRow
        <$> some cell
        <* blankline'
      where
        cell = do
          hspace
          char '|' $> mempty
            <|> withMContext
              (\c -> not $ isSpace c || c == '|')
              end
              (plainMarkupContext standardSet)
        end = try $ hspace >> void (char '|') <|> lookAhead newline'
