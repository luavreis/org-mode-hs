-- |

module Org.Parser.Elements where

import Prelude hiding (many, some)
import Org.Parser.Definitions
import Org.Parser.Common
import Org.Parser.Objects
import Org.Parser.MarkupContexts
import Relude.Extra hiding (next, elems)
import qualified Org.Builder as B
import qualified Data.Text as T
import Text.Slugify (slugify)

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
  _ <- blankline' $> ""
       <|> char ' ' *> anyLine'
  pure mempty

elements :: OrgParser (F OrgElements)
elements = elements' (void (lookAhead headingStart) <|> eof)

elements' :: OrgParser end -> OrgParser (F OrgElements)
elements' end = mconcat <$> manyTill (element <|> para) end

-- | Each element parser must consume till the start of a line or EOF.
-- This is necessary for correct counting of list indentations.
element :: OrgParser (F OrgElements)
element = choice [ blankline $> mempty <* clearPendingAffiliated
                 , elementNonEmpty
                 ] <?> "org element or blank line"

elementNonEmpty :: OrgParser (F OrgElements)
elementNonEmpty = choice [ commentLine      <* clearPendingAffiliated
                         , exampleBlock     <* clearPendingAffiliated
                         , srcBlock         <* clearPendingAffiliated
                         , exportBlock      <* clearPendingAffiliated
                         , greaterBlock     <* clearPendingAffiliated
                         , plainList        <* clearPendingAffiliated
                         , latexEnvironment <* clearPendingAffiliated
                         , drawer           <* clearPendingAffiliated
                         , affKeyword
                         , keyword          <* clearPendingAffiliated
                         ] <?> "org element"

para :: OrgParser (F OrgElements)
para = try do
  hspace
  f <- withAffiliated B.para
  (inls, next) <- withMContext_
                  (mark "\n" end)
                  (plainMarkupContext standardSet)
  pure $ (f <*> inls) <> next
  where
    end :: OrgParser (F OrgElements)
    end = try do
      _ <- newline'
      lookAhead headingStart $> mempty
        <|> eof $> mempty
        <|> element


-- * Plain lists

plainList :: OrgParser (F OrgElements)
plainList = try do
  f <- withAffiliated B.list
  ((indent, fstItem), i0) <- runStateT listItem 0
  rest <- evalStateT (many . try $ guardIndent indent =<< listItem) i0
  let kind = listItemType <$> fstItem
      items = (:) <$> fstItem <*> sequence rest
  pure $ f <*> kind <*> items
  where
    guardIndent indent (i, l) = guard (indent == i) $> l

listItem :: StateT Int OrgParser (Int, F ListItem)
listItem = try do
  (indent, bullet) <- lift $ unorderedBullet <|> counterBullet
  hspace1 <|> lookAhead (void newline')
  cookie <- lift $ optional counterSet
  box    <- lift $ optional checkbox
  case cookie of
    Just n0 -> put n0
    Nothing -> modify (+ 1)
  n <- B.plain . show <$> get
  lift $ withTargetDescription (pure n) do
    tag <- case bullet of
      Bullet _ -> toList <<$>> option mempty itemTag
      _        -> pureF []
    els <- liftA2 (<>) (blankline' $> mempty <|> indentedPara indent)
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
  <* (hspace1 <|> lookAhead (void newline'))
  where
    tick = char ' ' $> BoolBox False <|>
           char 'X' $> BoolBox True  <|>
           char '-' $> PartialBox

itemTag :: OrgParser (F OrgObjects)
itemTag = try do
  clearLastChar
  st <- getFullState
  (contents, found) <- findMarked end
  guard found
  parseFromText st contents (plainMarkupContext standardSet)
  where
    end = mark " \t\n" $
      spaceOrTab *> string "::" *> spaceOrTab $> True
        <|> newline' $> False

indentedPara :: Int -> OrgParser (F OrgElements)
indentedPara indent = try do
  hspace
  f <- withAffiliated B.para
  (inls, next) <- withMContext_
                  (mark "\n" end)
                  (plainMarkupContext standardSet)
  pure $ (f <*> inls) <> next
  where
    end :: OrgParser (F OrgElements)
    end = try do
      _ <- newline'
      lookAhead blankline' $> mempty
        <|> lookAhead headingStart $> mempty
        -- We don't want to consume any indentation, so we look ahead.
        <|> lookAhead (try $ guard . (<= indent) =<< spacesOrTabs) $> mempty
        <|> element

indentedElements :: Int -> OrgParser (F OrgElements)
indentedElements indent =
  mconcat <$> many indentedElement
  where
    indentedElement = try do
      notFollowedBy headingStart
      blankline
        *> notFollowedBy blankline'
        *> clearPendingAffiliated $> mempty
        <|> do
        guard . (> indent) =<< lookAhead spacesOrTabs
        elementNonEmpty <|> indentedPara indent


-- * Lesser blocks

exampleBlock :: OrgParser (F OrgElements)
exampleBlock = try do
  f <- withAffiliated B.example
  hspace
  _ <- string' "#+begin_example"
  switches <- blockSwitches
  _ <- anyLine
  startingNumber <- updateLineNumbers switches
  contents <- rawBlockContents end switches
  pure $ f ?? startingNumber ?? contents
  where
    end = try $ hspace *> string' "#+end_example" <* blankline'

srcBlock :: OrgParser (F OrgElements)
srcBlock = try do
  f <- withAffiliated B.srcBlock
  hspace
  _ <- string' "#+begin_src"
  lang <- option "" $ hspace1 *> someNonSpace
  switches <- blockSwitches
  args <- headerArgs
  num <- updateLineNumbers switches
  contents <- rawBlockContents end switches
  pure $ f ?? lang ?? num ?? args ?? contents
  where
    end = try $ hspace *> string' "#+end_src" <* blankline'

headerArgs :: StateT OrgParserState Parser [(Text, Text)]
headerArgs = do
  hspace
  fromList <$> headerArg `sepBy` hspace1
    <* anyLine'
  where
    headerArg = liftA2 (,) (char ':' *> someNonSpace)
                           (T.strip . fst <$>
                            findMarked (mark " \n" $ try $
                                        lookAhead (newline' <|>
                                                   hspace1 <* char ':')))

exportBlock :: OrgParser (F OrgElements)
exportBlock = try do
  hspace
  _ <- string' "#+begin_export"
  format <- option "" $ hspace1 *> someNonSpace
  _ <- anyLine
  contents <- T.unlines <$> manyTill anyLine end
  pureF $ B.export format contents
  where
    end = try $ hspace *> string' "#+end_export" <* blankline'

-- verseBlock :: OrgParser (F OrgElements)
-- verseBlock = try do
--   hspace
--   _ <- string' "#+begin_verse"
--   undefined
--   where
    -- end = try $ hspace *> string' "#+end_export" <* blankline'


indentContents :: Int -> [SrcLine] -> [SrcLine]
indentContents tabWidth (map (srcLineMap $ tabsToSpaces tabWidth) -> lins) =
  map (srcLineMap $ T.drop minIndent) lins
  where
    minIndent = maybe 0 minimum1 (nonEmpty $ map (indentSize . srcLineContent) lins)
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
    Just (readMaybe . toString -> n) -> setSrcLineNum (fromMaybe 1 n)
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
                 (satisfy $ \c -> isAsciiAlpha c || isDigit c || c == '-' || c == ' ')
                 (string $ T.reverse refpre)
          content <- T.stripEnd . T.reverse <$> takeInput
          pure (ref, content)
        = do
          (ref', alias) <- if "-r" `member` switches
                           then ("",) . show <$> getSrcLineNum
                           else pure (ref, ref)
          let anchor = "coderef-" <> slugify ref
          registerAnchorTarget ("(" <> ref <> ")") anchor (pure $ B.plain alias)
          pure $ RefLine anchor ref' content
      | otherwise = pure $ SrcLine txt

blockSwitches :: OrgParser (Map Text Text)
blockSwitches = fromList <$> many (linum <|> switch <|> fmt)
  where
    linum :: OrgParser (Text, Text)
    linum = try $ do
      hspace1
      s <- T.snoc . one <$> oneOf ['+', '-']
                        <*> char 'n'
      num <- option "" $ try $ hspace1 *> takeWhileP Nothing isDigit
      _ <- lookAhead spaceChar
      return (s, num)

    fmt :: OrgParser (Text, Text)
    fmt = try $ do
      hspace1
      s <- string "-l"
      hspace1
      str <- between (char '"') (char '"') $
             takeWhileP Nothing (\c -> c /= '"' && c /= '\n')
      _ <- lookAhead spaceChar
      return (s, str)

    switch :: OrgParser (Text, Text)
    switch = try $ do
      hspace1
      s <- T.snoc . one <$> char '-'
                        <*> oneOf ['i', 'k', 'r']
      _ <- lookAhead spaceChar
      pure (s, "")


-- * Greater Blocks

greaterBlock :: OrgParser (F OrgElements)
greaterBlock = try do
  f <- withAffiliated B.greaterBlock
  hspace
  _ <- string' "#+begin_"
  bname <- someNonSpace
  _ <- takeWhileP Nothing (/= '\n')
  els <- withMContext (end bname) elements
  clearPendingAffiliated
  pure $ f ?? blockType bname <*> els
  where
    blockType = \case
      (T.toLower -> "center") -> Center
      (T.toLower -> "quote") -> Quote
      other -> Special other
    end :: Text -> Marked OrgParser Text
    end name = mark "\n" . try $ newline *> hspace *> string' "#+end_" *> string' name <* blankline'

-- * Drawers

drawer :: OrgParser (F OrgElements)
drawer = try do
  hspace
  _ <- char ':'
  dname <- takeWhile1P (Just "drawer name") (\c -> c /= ':' && c /= '\n')
  hspace <* lookAhead newline
  els <- withMContext end elements
  return $ B.drawer dname <$> els
  where
    end :: Marked OrgParser ()
    end = mark "\n" . try $ newline *> hspace <* string' ":end:"


-- * LaTeX Environments

latexEnvironment :: OrgParser (F OrgElements)
latexEnvironment = try do
  hspace
  _ <- string "\\begin{"
  ename <- takeWhile1P (Just "latex environment name")
                       (\c -> isAsciiAlpha c || isDigit c || c == '*')
  _ <- char '}'
  (str, _) <- findMarked (end ename)
  f <- withAffiliated B.latexEnvironment
  pure $ f ?? ename ?? "\\begin{" <> ename <> "}" <> str <> "\\end{" <> ename <> "}"
  where
    end :: Text -> Marked OrgParser ()
    end name = mark "\\" . try $ string ("\\end{" <> name <> "}") *> blankline'

-- * Keywords and affiliated keywords

affKeyword :: OrgParser (F OrgElements)
affKeyword = try do
  hspace
  _ <- string "#+"
  try do
      (T.toLower -> name) <- liftA2 (<>)
                             (string' "attr_")
                             (takeWhile1P Nothing (\c -> not (isSpace c || c == ':')))
      _ <- char ':'
      args <- headerArgs
      registerAffiliated $ pure (name, B.attrKeyword args)
      pure mempty
    <|> try do
      affkws <- getsO orgElementAffiliatedKeywords
      name <- choice (fmap (\s -> string' s $> s) affkws)
      isdualkw <- (name `elem`) <$> getsO orgElementDualKeywords
      isparsedkw <- (name `elem`) <$> getsO orgElementParsedKeywords
      value <- if isparsedkw
        then do
          optArg <- option (pure mempty) $ guard isdualkw *> optionalArgP
          _ <- char ':'
          hspace
          value <- withMContext (mark' '\n' newline') (plainMarkupContext standardSet)
          pure $ B.parsedKeyword <$> optArg <*> value
        else do
          optArg <- option "" $ guard isdualkw *> optionalArg
          _ <- char ':'
          hspace
          pure . B.valueKeyword optArg . T.stripEnd <$> anyLine'
      registerAffiliated $ (name,) <$> value
      pure mempty
  where
    optionalArgP = withBalancedContext '[' ']' (All . (/= '\n') <> All . (/= ':')) $
                   plainMarkupContext standardSet
    optionalArg = withBalancedContext '[' ']' (All . (/= '\n') <> All . (/= ':'))
                  takeInput

keyword :: OrgParser (F OrgElements)
keyword = try do
  hspace
  _ <- string "#+"
  -- This is one of the places where it is convoluted to replicate org-element
  -- regexes: "#+abc:d:e :f" is a valid keyword of key "abc:d" and value "e :f".
  name <- T.toLower . fst <$> fix \me -> do -- a joke?
    res@(name, ended) <- findMarked $
      Marked (ap2 (||) isSpace (== ':')) ["space", ":"] . try $
        (newline' <|> void (satisfy isSpace)) $> False
        <|> char ':' *> notFollowedBy me $> True
    guard (not $ T.null name)
    guard ended <?> "keyword end"
    pure res
  hspace
  parsedkw <- (name `elem`) <$> getsO orgElementParsedKeywords
  value <-
    if parsedkw
    then B.parsedKeyword' <<$>> withMContext (mark' '\n' newline') (plainMarkupContext standardSet)
    else pure . B.valueKeyword' . T.stripEnd <$> anyLine'
  let kw = (name,) <$> value
  registerKeyword kw
  return $ uncurry B.keyword <$> kw
