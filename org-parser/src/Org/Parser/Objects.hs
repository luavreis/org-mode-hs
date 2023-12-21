-- | Parsers for Org objects.
module Org.Parser.Objects
  ( -- * Sets of markup
    minimalSet
  , standardSet

    -- * Marked parsers
  , Marked (..)
  , markupContext
  , plainMarkupContext

    -- * General purpose parsers
  , markup
  , rawMarkup

    -- * Objects
  , code
  , verbatim
  , italic
  , underline
  , bold
  , strikethrough
  , singleQuoted
  , doubleQuoted
  , entity
  , latexFragment
  , texMathFragment
  , exportSnippet
  , citation
  , inlBabel
  , inlSrc
  , linebreak
  , angleLink
  , regularLink
  , target
  , suscript
  , macro
  , footnoteReference
  , timestamp
  , statisticCookie

    -- * Auxiliary
  , linkToTarget
  , parseTimestamp
  )
where

import Data.Set qualified as Set
import Data.Text qualified as T
import Org.Data.Entities (defaultEntitiesNames)
import Org.Parser.Common
import Org.Parser.Definitions
import Org.Parser.MarkupContexts
import Prelude hiding (many, some)

-- * Sets of objects

minimalSet :: Marked OrgParser OrgObjects
minimalSet =
  mconcat
    [ endline
    , code
    , verbatim
    , italic
    , underline
    , bold
    , strikethrough
    , entity
    , latexFragment
    , texMathFragment
    , singleQuoted
    , doubleQuoted
    , suscript
    , statisticCookie
    , macro
    ]

standardSet :: Marked OrgParser OrgObjects
standardSet =
  mconcat
    [ minimalSet
    , regularLink
    , footnoteReference
    , timestamp
    , exportSnippet
    , inlBabel
    , inlSrc
    , linebreak
    , target
    , angleLink
    , citation
    ]

{- | Parse inside a "plain context", i.e., plain text not matched by any parsers
   gets converted to 'Plain' objects.

@
'plainMarkupContext' = 'markupContext' 'B.plain'
@
-}
plainMarkupContext :: Marked OrgParser OrgObjects -> OrgParser OrgObjects
plainMarkupContext = markupContext Plain

newlineAndClear :: OrgParser Char
newlineAndClear = newline <* clearLastChar

emphasisPreChars :: Set Char
emphasisPreChars = fromList "-('\"{"

emphPreChar :: Char -> Bool
emphPreChar c = isSpace c || c `Set.member` emphasisPreChars

emphasisPre :: Char -> OrgParser ()
emphasisPre s = try $ do
  lchar <- gets (.lastChar)
  for_ lchar $ guard . emphPreChar
  _ <- char s
  notFollowedBy spaceChar

emphasisPostChars :: Set Char
emphasisPostChars = fromList "-.,;:!?'\")}\\["

emphPostChar :: Char -> Bool
emphPostChar c = isSpace c || c `Set.member` emphasisPostChars

emphasisPost :: Char -> OrgParser ()
emphasisPost e = try $ do
  lchar <- gets (.lastChar)
  for_ lchar $ guard . not . isSpace
  _ <- char e
  putLastChar e
  lookAhead (eof <|> void (satisfy emphPostChar))

emphasisSkip :: Char -> OrgParser ()
emphasisSkip s = try $ do
  putLastChar =<< anySingle
  t <- takeWhileP Nothing (/= s)
  setLastChar (snd <$> T.unsnoc t)

markup ::
  (OrgObjects -> OrgObjects) ->
  Char ->
  Marked OrgParser OrgObjects
markup f c = Marked [c]
  $ try
  $ do
    emphasisPre c
    st <- getFullState
    s <- anySingle
    (t, _) <- skipManyTill' (emphasisSkip c) (emphasisPost c)
    f <$> parseFromText st (T.cons s t) (plainMarkupContext standardSet)

rawMarkup ::
  (Text -> OrgObjects) ->
  Char ->
  Marked OrgParser OrgObjects
rawMarkup f d = Marked [d]
  $ try
  $ do
    emphasisPre d
    f . fst <$> skipManyTill' (emphasisSkip d) (emphasisPost d)

-- | Parse a code object.
code :: Marked OrgParser OrgObjects
code = rawMarkup B.code '~'

-- | Parse a verbatim object.
verbatim :: Marked OrgParser OrgObjects
verbatim = rawMarkup B.verbatim '='

-- | Parse an italic object.
italic :: Marked OrgParser OrgObjects
italic = markup B.italic '/'

-- | Parse an underline object.
underline :: Marked OrgParser OrgObjects
underline = markup B.underline '_'

-- | Parse a bold object.
bold :: Marked OrgParser OrgObjects
bold = markup B.bold '*'

-- | Parse a strikethrough object.
strikethrough :: Marked OrgParser OrgObjects
strikethrough = markup B.strikethrough '+'

-- | Parse a single-quoted object.
singleQuoted :: Marked OrgParser OrgObjects
singleQuoted = markup B.singleQuoted '\''

-- | Parse a double-quoted object.
doubleQuoted :: Marked OrgParser OrgObjects
doubleQuoted = markup B.doubleQuoted '"'

-- | An endline character that can be treated as a space, not a line break.
endline :: Marked OrgParser OrgObjects
endline =
  Marked "\n"
    $ try
    $ newlineAndClear
    *> hspace
    $> B.object (B.plain "\n")

-- * Entities and LaTeX fragments

-- | Parse an entity object.
entity :: Marked OrgParser OrgObjects
entity = Marked "\\" $ try $ do
  _ <- char '\\'
  name <- choice (map string defaultEntitiesNames)
  void (string "{}") <|> notFollowedBy asciiAlpha
  pure $ B.object $ B.entity name

-- | Parse a LaTeX fragment object.
latexFragment :: Marked OrgParser OrgObjects
latexFragment = Marked "\\" $ try do
  _ <- char '\\'
  mathFragment <|> rawFragment
  where
    mathFragment = try do
      inline <-
        char '('
          $> True
          <|> char '['
          $> False
      (str, _) <-
        findSkipping
          (/= '\\')
          (try $ char '\\' *> char if inline then ')' else ']')
      pure
        $ B.object
        $ if inline
          then B.inlMath str
          else B.dispMath str

    rawFragment :: (MonadParser m) => m OrgObjects
    rawFragment = try $ do
      name <- someAsciiAlpha
      text <- (name <>) <$> option "" brackets
      pure $ B.object $ B.fragment ("\\" <> text)

    brackets :: (MonadParser m) => m Text
    brackets = try $ do
      open <- satisfy (\c -> c == '{' || c == '[')
      let close = if open == '{' then '}' else ']'
      str <- takeWhileP Nothing (\c -> c /= close && c /= '\n')
      _ <- char close
      pure $ open `T.cons` str `T.snoc` close

-- | Parse a TeX math fragment object.
texMathFragment :: Marked OrgParser OrgObjects
texMathFragment = Marked "$" $ try $ display <|> inline
  where
    display = try $ do
      _ <- string "$$"
      (str, _) <-
        findSkipping
          (/= '$')
          (string "$$")
      pure $ B.object $ B.dispMath str

    post = do
      _ <- char '$'
      eof
        <|> ( void
                . lookAhead
                $ satisfy (\x -> isPunctuation x || isSpace x || x == '"')
            )

    inline = try $ do
      lchar <- gets (.lastChar)
      for_ lchar $ guard . (/= '$')
      _ <- char '$'
      str <- singleChar <|> moreChars
      pure $ B.object $ B.inlMath str

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

-- | Parse an export snippet object.
exportSnippet :: Marked OrgParser OrgObjects
exportSnippet = Marked "@"
  $ try
  $ do
    _ <- string "@@"
    backend <-
      takeWhile1P
        (Just "export snippet backend")
        (\c -> isAsciiAlpha c || isDigit c || c == '-')
    _ <- char ':'
    B.object
      . B.exportSnippet backend
      . fst
      <$> findSkipping (/= '@') (string "@@")

-- * Citations

-- The following code for org-cite citations was adapted and improved upon pandoc's.

-- | Parse a citation object.
citation :: Marked OrgParser OrgObjects
citation =
  Marked "["
    $ B.object
    . B.citation
    <$> withBalancedContext '[' ']' (const True) orgCite

-- | A citation in org-cite style
orgCite :: OrgParser (Citation OrgObjects)
orgCite = try $ do
  _ <- string "cite"
  (style, variant) <- citeStyle
  _ <- char ':'
  space
  globalPrefix <- option mempty (try (citeSuffix <* char ';'))
  items <- citeItems
  globalSuffix <- option mempty (try (char ';' *> citePrefix))
  space
  eof
  return
    Citation
      { style = style
      , variant = variant
      , prefix = toList globalPrefix
      , suffix = toList globalSuffix
      , references = items
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

citeItems :: OrgParser [CiteReference OrgObjects]
citeItems = citeItem `sepBy1'` char ';'
  where
    sepBy1' p sep = (:) <$> p <*> many (try $ sep >> p)

citeItem :: OrgParser (CiteReference OrgObjects)
citeItem = do
  pref <- option mempty citePrefix
  itemKey <- orgCiteKey
  suff <- option mempty citeSuffix
  return
    CiteReference
      { id = itemKey
      , prefix = toList pref
      , suffix = toList suff
      }

citePrefix :: OrgParser OrgObjects
citePrefix = try $ do
  clearLastChar
  withContext
    (takeWhile1P Nothing (/= '@'))
    (eof <|> void (lookAhead $ char '@'))
    (plainMarkupContext minimalSet)

citeSuffix :: OrgParser OrgObjects
citeSuffix = try $ do
  clearLastChar
  withContext
    (takeWhile1P Nothing (/= ';'))
    (eof <|> void (lookAhead $ char ';'))
    (plainMarkupContext minimalSet)

orgCiteKey :: OrgParser Text
orgCiteKey = do
  _ <- char '@'
  takeWhile1P (Just "citation key allowed chars") orgCiteKeyChar

orgCiteKeyChar :: Char -> Bool
orgCiteKeyChar c =
  isAlphaNum c || c `elem` (".:?!`\'/*@+|(){}<>&_^$#%~-" :: String)

-- * Inline Babel calls

-- | Parse an inline babel call object.
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
  return $ B.object $ B.inlBabel name header1 header2 args
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    arguments = withBalancedContext '(' ')' (/= '\n') getInput

-- * Inline source blocks

-- | Parse an inline source object.
inlSrc :: Marked OrgParser OrgObjects
inlSrc = Marked "s" . try $ do
  _ <- string "src_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c /= '{' && c /= '[')
  headers <- option "" header
  B.object . B.inlSrc name headers <$> body
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    body = withBalancedContext '{' '}' (/= '\n') getInput

-- * Line breaks

-- | Parse a linebreak object.
linebreak :: Marked OrgParser OrgObjects
linebreak =
  Marked "\\"
    . try
    $ B.object B.linebreak
    <$ string "\\\\"
    <* blankline'
    <* clearLastChar

-- * Links

-- | Parse a angle link object.
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
    char '>'
      $> partial
      <|> newline
      *> hspace
      *> ((T.stripEnd partial <>) <$> search)
  return $ B.object $ B.uriLink protocol tgt (B.object $ B.plain $ protocol <> ":" <> tgt)

-- | Parse a regular link object.
regularLink :: Marked OrgParser OrgObjects
regularLink =
  Marked "["
    . try
    $ do
      _ <- string "[["
      str <- linkTarget
      descr <- linkDescr <|> char ']' $> mempty
      putLastChar ']'
      return $ B.object $ B.link (linkToTarget str) descr
  where
    linkTarget :: (MonadParser m) => m Text
    linkTarget = fix $ \rest -> do
      partial <-
        takeWhileP
          (Just "link target")
          (\c -> c /= ']' && c /= '[' && c /= '\\' && c /= '\n')
      oneOf ['[', ']']
        $> partial
        <|> char '\\'
        *> liftA2 T.cons (option '\\' $ oneOf ['[', ']']) rest
        <|> newline
        *> hspace
        *> ((T.stripEnd partial `T.snoc` ' ' <>) <$> rest)

    linkDescr :: OrgParser OrgObjects
    linkDescr = try $ do
      _ <- char '['
      -- FIXME this is not the right set but... whatever
      withContext skip (string "]]") (plainMarkupContext standardSet)
      where
        skip = anySingle >> takeWhileP Nothing (/= ']')

-- TODO this will probably be replaced by the AST annotations.

-- | Transform the link text into a link target.
linkToTarget :: Text -> LinkTarget
linkToTarget link
  | any (`T.isPrefixOf` link) ["/", "./", "../"] =
      let link' = toText (toString link)
       in URILink "file" link'
  | (prot, rest) <- T.break (== ':') link
  , Just (_, uri) <- T.uncons rest =
      URILink prot uri
  | otherwise = UnresolvedLink link

-- * Targets and radio targets

-- | Parse a target object.
target :: Marked OrgParser OrgObjects
target = Marked "<" $ try do
  _ <- string "<<"
  str <- takeWhile1P (Just "dedicated target") (\c -> c /= '<' && c /= '>' && c /= '\n')
  guard (not (isSpace $ T.head str))
  guard (not (isSpace $ T.last str))
  _ <- string ">>"
  return $ B.object $ B.target str

-- * Subscripts and superscripts

-- | Parse a subscript or a superscript object.
suscript :: Marked OrgParser OrgObjects
suscript = Marked "_^" $ try do
  lchar <- gets (.lastChar)
  for_ lchar $ guard . not . isSpace
  start <- satisfy \c -> c == '_' || c == '^'
  contents <- asterisk <|> balanced <|> plain
  pure
    $ if start == '_'
      then B.object $ B.subscript contents
      else B.object $ B.superscript contents
  where
    asterisk = B.object . B.plain . one <$> char '*'

    balanced =
      withBalancedContext '{' '}' (const True)
        $ plainMarkupContext minimalSet

    sign = option mempty (B.object . B.plain . one <$> oneOf ['+', '-'])

    plain =
      liftA2 (<>) sign
        $ withMContext (const True) isAlphaNum plainEnd
        $ plainMarkupContext (entity <> latexFragment)

    plainEnd :: OrgParser ()
    plainEnd = try do
      lookAhead
        $ eof
        <|> try (some (oneOf [',', '.', '\\']) *> notFollowedBy (satisfy isAlphaNum))
        <|> void (noneOf [',', '.', '\\'])

-- * Macros

-- | Parse a macro object.
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
  return $ B.object $ B.macro key args
  where
    allowedKeyChar c = isAsciiAlpha c || isDigit c || c == '-' || c == '_'

-- * Footnote references

-- | Parse a footnote reference object.
footnoteReference :: Marked OrgParser OrgObjects
footnoteReference = Marked "["
  $ withBalancedContext '[' ']' (const True) do
    _ <- string "fn:"
    lbl <-
      optional
        $ takeWhile1P
          (Just "footnote ref label")
          (\c -> isAlphaNum c || c == '-' || c == '_')
    def <-
      optional $ try do
        _ <- char ':'
        plainMarkupContext standardSet
    case (lbl, def) of
      (Nothing, Nothing) -> empty
      (Just lbl', Nothing) ->
        return $ B.object $ B.footnoteLabel lbl'
      (_, Just def') ->
        return $ B.object $ B.footnoteInlDef lbl def'

-- * Timestamps

-- | Parse a timestamp object.
timestamp :: Marked OrgParser OrgObjects
timestamp = Marked "<[" $ B.object . B.timestamp <$> parseTimestamp

-- | Parse a timestamp.
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

    parseDate :: OrgParser OrgDate
    parseDate = do
      year <- number 4 <* char '-'
      month <- number 2 <* char '-'
      day <- number 2
      dayName <- optional $ try do
        hspace1
        takeWhile1P (Just "dayname characters") isLetter
      pure $ OrgDate year month day dayName

    repeaterMark = tsmark ["++", ".+", "+"]

    warningMark = tsmark ["--", "-"]

    tsmark :: [Text] -> OrgParser TimestampMark
    tsmark marks = do
      mtype <- (,,) <$> choice (map string marks)
      mtype <$> integer <*> oneOf ['h', 'd', 'w', 'm', 'y']

-- * Statistic Cookies

-- | Parse a statistic cookie object.
statisticCookie :: Marked OrgParser OrgObjects
statisticCookie = Marked "[" $ try do
  _ <- char '['
  res <- Left <$> fra <|> Right <$> pct
  _ <- char ']'
  return $ B.object $ B.statisticCookie res
  where
    fra = try $ liftA2 (,) integer (char '/' *> integer)
    pct = try $ integer <* char '%'
