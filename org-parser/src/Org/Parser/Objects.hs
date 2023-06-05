module Org.Parser.Objects where

import Data.Text qualified as T
import Org.Builder qualified as B
import Org.Data.Entities (defaultEntitiesNames)
import Org.Parser.Common
import Org.Parser.Definitions
import Org.Parser.MarkupContexts
import Prelude hiding (many, some)

-- * Sets of objects

minimalSet :: Marked OrgParser OrgObjects
minimalSet =
  mconcat
    [ endline,
      code,
      verbatim,
      italic,
      underline,
      bold,
      striketrough,
      mathFragment,
      texMathFragment,
      entityOrFragment,
      singleQuoted,
      doubleQuoted,
      suscript,
      statisticCookie,
      macro
    ]

standardSet :: Marked OrgParser OrgObjects
standardSet =
  mconcat
    [ minimalSet,
      regularLinkOrImage,
      footnoteReference,
      timestamp,
      exportSnippet,
      inlBabel,
      inlSrc,
      linebreak,
      target,
      angleLink,
      citation
    ]

plainMarkupContext :: Marked OrgParser OrgObjects -> OrgParser OrgObjects
plainMarkupContext = markupContext B.plain

newlineAndClear :: OrgParser Char
newlineAndClear = newline <* clearLastChar

newlineAndClear' :: OrgParser ()
newlineAndClear' = newline' <* clearLastChar

emphasisPreChars :: String
emphasisPreChars = "-\t\n ('\"{\8203"

emphasisPostChars :: String
emphasisPostChars = " ,.-\t\n:!?;'\")}[\8203"

emphasisPost :: Char -> OrgParser ()
emphasisPost e = try $ do
  lchar <- gets orgStateLastChar
  for_ lchar $ guard . not . isSpace
  _ <- char e
  putLastChar e
  lookAhead
    (eof <|> void (satisfy (`elem` emphasisPostChars)))

emphasisPre :: Char -> OrgParser ()
emphasisPre s = try $ do
  lchar <- gets orgStateLastChar
  for_ lchar $ guard . (`elem` emphasisPreChars)
  _ <- char s
  clearLastChar
  notFollowedBy spaceChar

markup ::
  (OrgObjects -> OrgObjects) ->
  Char ->
  Marked OrgParser OrgObjects
markup f c = Marked [c] $
  try $ do
    emphasisPre c
    f <$> withMContext (/= c) (emphasisPost c) (plainMarkupContext standardSet)

rawMarkup ::
  (Text -> OrgObjects) ->
  Char ->
  Marked OrgParser OrgObjects
rawMarkup f d = Marked [d] $
  try $ do
    emphasisPre d
    f <$> withMContext (/= d) (emphasisPost d) getInput

code :: Marked OrgParser OrgObjects
code = rawMarkup B.code '~'

verbatim :: Marked OrgParser OrgObjects
verbatim = rawMarkup B.verbatim '='

italic :: Marked OrgParser OrgObjects
italic = markup B.italic '/'

underline :: Marked OrgParser OrgObjects
underline = markup B.underline '_'

bold :: Marked OrgParser OrgObjects
bold = markup B.bold '*'

striketrough :: Marked OrgParser OrgObjects
striketrough = markup B.strikethrough '+'

singleQuoted :: Marked OrgParser OrgObjects
singleQuoted = markup B.singleQuoted '\''

doubleQuoted :: Marked OrgParser OrgObjects
doubleQuoted = markup B.doubleQuoted '"'

-- | An endline character that can be treated as a space, not a line break.
endline :: Marked OrgParser OrgObjects
endline =
  Marked "\n" $
    try $
      newlineAndClear
        *> hspace
        $> B.plain "\n"

-- * Entities and LaTeX fragments

entityOrFragment :: Marked OrgParser OrgObjects
entityOrFragment = Marked "\\" $
  try $ do
    _ <- char '\\'
    entity <|> fragment
  where
    entity :: MonadParser m => m OrgObjects
    entity = try $ do
      name <- choice (map string defaultEntitiesNames)
      void (string "{}") <|> notFollowedBy asciiAlpha
      pure $ B.entity name

    fragment :: MonadParser m => m OrgObjects
    fragment = try $ do
      name <- someAsciiAlpha
      text <- (name <>) <$> option "" brackets
      pure $ B.fragment ("\\" <> text)

    brackets :: MonadParser m => m Text
    brackets = try $ do
      open <- satisfy (\c -> c == '{' || c == '[')
      let close = if open == '{' then '}' else ']'
      str <- takeWhileP Nothing (\c -> c /= close && c /= '\n')
      _ <- char close
      pure $ open `T.cons` str `T.snoc` close

mathFragment :: Marked OrgParser OrgObjects
mathFragment = Marked "\\" $
  try $ do
    _ <- char '\\'
    inline <-
      char '(' $> True
        <|> char '[' $> False
    (str, _) <-
      findSkipping
        (/= '\\')
        (try $ char '\\' *> char if inline then ')' else ']')
    pure $
      if inline
        then B.inlMath str
        else B.dispMath str

texMathFragment :: Marked OrgParser OrgObjects
texMathFragment = Marked "$" $ try $ display <|> inline
  where
    display = try $ do
      _ <- string "$$"
      (str, _) <-
        findSkipping
          (/= '$')
          (string "$$")
      pure $ B.dispMath str

    post = do
      _ <- char '$'
      eof
        <|> ( void . lookAhead $
                satisfy (\x -> isPunctuation x || isSpace x || x == '"')
            )

    inline = try $ do
      lchar <- gets orgStateLastChar
      for_ lchar $ guard . (/= '$')
      _ <- char '$'
      str <- singleChar <|> moreChars
      pure $ B.inlMath str

    moreChars = try $ do
      str <- takeWhile1P (Just "inside of inline math") (/= '$')
      guard $ border1 (T.head str) && border2 (T.last str)
      post
      pure str

    singleChar = try $ do
      c <- satisfy (\x -> not (isSpace x) && x `notElem` disallowedCharsSet)
      post
      pure $ one c

    disallowedCharsSet :: [Char]
    disallowedCharsSet = ['.', ',', '?', ';', '"']

    border1 c = not (isSpace c) && c `notElem` (".,;$" :: String)
    border2 c = not (isSpace c) && c `notElem` (".,$" :: String)

-- * Export snippets

exportSnippet :: Marked OrgParser OrgObjects
exportSnippet = Marked "@" $
  try $ do
    _ <- string "@@"
    backend <-
      takeWhile1P
        (Just "export snippet backend")
        (\c -> isAsciiAlpha c || isDigit c || c == '-')
    _ <- char ':'
    B.exportSnippet backend . fst
      <$> findSkipping (/= '@') (string "@@")

-- * Citations

-- The following code for org-cite citations was adapted and improved upon pandoc's.

citation :: Marked OrgParser OrgObjects
citation =
  Marked "[" $
    B.citation <$> withBalancedContext '[' ']' (const True) orgCite

-- | A citation in org-cite style
orgCite :: OrgParser Citation
orgCite = try $ do
  _ <- string "cite"
  (style, variant) <- citeStyle
  _ <- char ':'
  space
  globalPrefix <- option mempty (try (citePrefix <* char ';'))
  items <- citeItems
  globalSuffix <- option mempty (try (char ';' *> citeSuffix))
  space
  eof
  return
    Citation
      { citationStyle = style,
        citationVariant = variant,
        citationPrefix = toList globalPrefix,
        citationSuffix = toList globalSuffix,
        citationReferences = items
      }

citeStyle :: OrgParser (Tokens Text, Tokens Text)
citeStyle = do
  sty <- option "" $ try style
  vars <- option "" $ try variants
  return (sty, vars)
  where
    style =
      char '/'
        *> takeWhileP
          (Just "alphaNum, '_' or '-' characters")
          (\c -> isAlphaNum c || c == '_' || c == '-')
    variants =
      char '/'
        *> takeWhileP
          (Just "alphaNum, '_', '-' or '/' characters")
          (\c -> isAlphaNum c || c == '_' || c == '-' || c == '/')

citeItems :: OrgParser [CiteReference]
citeItems = citeItem `sepBy1'` char ';'
  where
    sepBy1' p sep = (:) <$> p <*> many (try $ sep >> p)

citeItem :: OrgParser CiteReference
citeItem = do
  pref <- option mempty citePrefix
  itemKey <- orgCiteKey
  suff <- option mempty citeSuffix
  return
    CiteReference
      { refId = itemKey,
        refPrefix = toList pref,
        refSuffix = toList suff
      }

citePrefix :: OrgParser OrgObjects
citePrefix = try $ do
  clearLastChar
  withMContext
    (\c -> c /= '@' && c /= ';')
    ( try $
        eof <|> void (lookAhead $ oneOf ['@', ';'])
    )
    (plainMarkupContext minimalSet)

citeSuffix :: OrgParser OrgObjects
citeSuffix = try $ do
  clearLastChar
  withMContext
    (/= ';')
    ( try $
        eof <|> void (lookAhead $ single ';')
    )
    (plainMarkupContext minimalSet)

orgCiteKey :: OrgParser Text
orgCiteKey = do
  _ <- char '@'
  takeWhile1P (Just "citation key allowed chars") orgCiteKeyChar

orgCiteKeyChar :: Char -> Bool
orgCiteKeyChar c =
  isAlphaNum c || c `elem` (".:?!`\'/*@+|(){}<>&_^$#%~-" :: String)

-- * Inline Babel calls

inlBabel :: Marked OrgParser OrgObjects
inlBabel = Marked "c" . try $ do
  _ <- string "call_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c `notElem` ['[', ']', '(', ')'])
  header1 <- option "" header
  args <- arguments
  header2 <- option "" header
  return $ B.inlBabel name header1 header2 args
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    arguments = withBalancedContext '(' ')' (/= '\n') getInput

-- * Inline source blocks

inlSrc :: Marked OrgParser OrgObjects
inlSrc = Marked "s" . try $ do
  _ <- string "src_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c /= '{' && c /= '[')
  headers <- option "" header
  B.inlSrc name headers <$> body
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    body = withBalancedContext '{' '}' (/= '\n') getInput

-- * Line breaks

linebreak :: Marked OrgParser OrgObjects
linebreak =
  Marked "\\" . try $
    B.linebreak <$ string "\\\\" <* blankline' <* clearLastChar

-- * Links

angleLink :: Marked OrgParser OrgObjects
angleLink = Marked "<" . try $ do
  _ <- char '<'
  protocol <- manyAsciiAlpha
  _ <- char ':'
  tgt <- fix $ \search -> do
    partial <-
      takeWhile1P
        (Just "angle link target")
        (\c -> c /= '\n' && c /= '>')
    char '>' $> partial
      <|> newline *> hspace *> ((T.stripEnd partial <>) <$> search)
  return $ B.uriLink protocol tgt (B.plain $ protocol <> ":" <> tgt)

regularLinkOrImage :: Marked OrgParser OrgObjects
regularLinkOrImage =
  Marked "[" . try $
    do
      _ <- string "[["
      str <- linkTarget
      descr <- linkDescr <|> char ']' $> mempty
      putLastChar ']'
      return $ B.link (linkToTarget str) descr
  where
    linkTarget :: MonadParser m => m Text
    linkTarget = fix $ \rest -> do
      partial <-
        takeWhileP
          (Just "link target")
          (\c -> c /= ']' && c /= '[' && c /= '\\' && c /= '\n')
      oneOf ['[', ']'] $> partial
        <|> char '\\' *> liftA2 T.cons (option '\\' $ oneOf ['[', ']']) rest
        <|> newline *> hspace *> ((T.stripEnd partial `T.snoc` ' ' <>) <$> rest)

    linkDescr :: OrgParser OrgObjects
    linkDescr = try $ do
      _ <- char '['
      -- FIXME this is not the right set but... whatever
      withMContext (/= ']') (string "]]") (plainMarkupContext standardSet)

linkToTarget :: Text -> LinkTarget
linkToTarget link
  | any (`T.isPrefixOf` link) ["/", "./", "../"] =
      let link' = toText (toString link)
       in URILink "file" link'
  | (prot, rest) <- T.break (== ':') link,
    Just (_, uri) <- T.uncons rest =
      URILink prot uri
  | otherwise = UnresolvedLink link

-- * Targets and radio targets

target :: Marked OrgParser OrgObjects
target = Marked "<" $ try do
  _ <- string "<<"
  str <- takeWhile1P (Just "dedicated target") (\c -> c /= '<' && c /= '>' && c /= '\n')
  guard (not (isSpace $ T.head str))
  guard (not (isSpace $ T.last str))
  _ <- string ">>"
  return $ B.target "" str

-- * Subscripts and superscripts

suscript :: Marked OrgParser OrgObjects
suscript = Marked "_^" $ try do
  lchar <- gets orgStateLastChar
  for_ lchar $ guard . not . isSpace
  start <- satisfy \c -> c == '_' || c == '^'
  contents <- asterisk <|> balanced <|> plain
  pure $
    if start == '_'
      then B.subscript contents
      else B.superscript contents
  where
    asterisk = B.plain . one <$> char '*'

    balanced =
      withBalancedContext '{' '}' (const True) $
        plainMarkupContext minimalSet

    sign = option mempty (B.plain . one <$> oneOf ['+', '-'])

    plain =
      liftA2 (<>) sign $
        withMContext isAlphaNum plainEnd $
          plainMarkupContext entityOrFragment

    plainEnd :: OrgParser ()
    plainEnd = try do
      lookAhead $
        eof
          <|> try (some (oneOf [',', '.', '\\']) *> notFollowedBy (satisfy isAlphaNum))
          <|> void (noneOf [',', '.', '\\'])

-- * Macros

macro :: Marked OrgParser OrgObjects
macro = Marked "{" $ try do
  _ <- string "{{{"
  _ <- lookAhead $ satisfy isAsciiAlpha
  key <- takeWhile1P Nothing allowedKeyChar
  args <-
    (string "}}}" $> []) <|> do
      _ <- char '('
      t <- fst <$> findSkipping (/= ')') (string ")}}}")
      return $ T.split (== ',') t
  return $ B.macro key args
  where
    allowedKeyChar c = isAsciiAlpha c || isDigit c || c == '-' || c == '_'

-- * Footnote references

footnoteReference :: Marked OrgParser OrgObjects
footnoteReference = Marked "[" $
  withBalancedContext '[' ']' (const True) do
    _ <- string "fn:"
    lbl <-
      optional $
        takeWhile1P
          (Just "footnote ref label")
          (\c -> isAlphaNum c || c == '-' || c == '_')
    def <-
      optional $ try do
        _ <- char ':'
        plainMarkupContext standardSet
    case (lbl, def) of
      (Nothing, Nothing) -> empty
      (Just lbl', Nothing) ->
        return $ B.footnoteLabel lbl'
      (_, Just def') ->
        return $ B.footnoteInlDef lbl def'

-- * Timestamps

timestamp :: Marked OrgParser OrgObjects
timestamp = Marked "<[" $ B.timestamp <$> parseTimestamp

-- | Read a timestamp.
parseTimestamp :: OrgParser TimestampData
parseTimestamp = try $ do
  openChar <- lookAhead $ satisfy (\c -> c == '<' || c == '[')
  let isActive = openChar == '<'
      closeChar = if isActive then '>' else ']'
      delims = (openChar, closeChar)
  (d1, t1, r1, w1) <- component delims
  optional (try $ string "--" *> component delims)
    >>= \case
      Just (d2, t2, r2, w2) ->
        pure $ TimestampRange isActive (d1, fst <$> t1, r1, w1) (d2, fst <$> t2, r2, w2)
      Nothing -> case t1 of
        Just (t1', Just t1'') ->
          pure $ TimestampRange isActive (d1, Just t1', r1, w1) (d1, Just t1'', r1, w1)
        _ ->
          pure $ TimestampData isActive (d1, fst <$> t1, r1, w1)
  where
    component delims = do
      _ <- char (fst delims)
      date <- parseDate
      time <- optional . try $ do
        hspace1
        startTime <- parseTime
        endTime <- optional . try $ char '-' *> parseTime
        pure (startTime, endTime)
      repeater <- optional (try $ hspace1 *> repeaterMark)
      warning <- optional (try $ hspace1 *> warningMark)
      hspace
      _ <- char (snd delims)
      pure (date, time, repeater, warning)

    parseDate :: OrgParser Date
    parseDate = do
      year <- number 4 <* char '-'
      month <- number 2 <* char '-'
      day <- number 2
      dayName <- optional $ try do
        hspace1
        takeWhile1P (Just "dayname characters") isLetter
      pure (year, month, day, dayName)

    parseTime :: OrgParser Time
    parseTime = do
      hour <- (number 2 <|> number 1) <* char ':'
      minute <- number 2
      pure (hour, minute)

    repeaterMark = tsmark ["++", ".+", "+"]

    warningMark = tsmark ["--", "-"]

    tsmark :: [Text] -> OrgParser TimestampMark
    tsmark marks = do
      mtype <- (,,) <$> choice (map string marks)
      mtype <$> integer <*> oneOf ['h', 'd', 'w', 'm', 'y']

-- * Statistic Cookies

statisticCookie :: Marked OrgParser OrgObjects
statisticCookie = Marked "[" $ try do
  _ <- char '['
  res <- Left <$> fra <|> Right <$> pct
  _ <- char ']'
  return $ B.statisticCookie res
  where
    fra = try $ liftA2 (,) integer (char '/' *> integer)
    pct = try $ integer <* char '%'
