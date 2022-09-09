module Org.Parser.Objects where

import Data.Text qualified as T
import Org.Builder qualified as B
import Org.Data.Entities (defaultEntitiesNames)
import Org.Parser.Common
import Org.Parser.Definitions
import Org.Parser.MarkupContexts
import Relude.Extra
import Prelude hiding (many, some)

-- * Sets of objects

minimalSet :: Marked OrgParser (F OrgObjects)
minimalSet =
  mconcat
    [ endline,
      code,
      verbatim,
      italic,
      underline,
      bold,
      striketrough,
      entityOrFragment,
      mathFragment,
      texMathFragment,
      singleQuoted,
      doubleQuoted,
      suscript,
      statisticCookie
    ]

standardSet :: Marked OrgParser (F OrgObjects)
standardSet =
  mconcat
    [ minimalSet,
      citation,
      timestamp,
      exportSnippet,
      inlBabel,
      inlSrc,
      linebreak,
      target,
      angleLink,
      regularLinkOrImage,
      footnoteReference
    ]

plainMarkupContext :: Marked OrgParser (F OrgObjects) -> OrgParser (F OrgObjects)
plainMarkupContext = markupContext (pure . B.plain)

newlineAndClear :: OrgParser Char
newlineAndClear = newline <* clearLastChar

newlineAndClear' :: OrgParser ()
newlineAndClear' = newline' <* clearLastChar

emphasisPreChars :: String
emphasisPreChars = "-\t ('\"{\8203"

emphasisPostChars :: String
emphasisPostChars = " ,.-\t\n:!?;'\")}[\8203"

emphasisPost :: Char -> Marked OrgParser ()
emphasisPost e = mark [e] $
  try $ do
    lchar <- gets orgStateLastChar
    for_ lchar $ guard . not . isSpace
    _ <- char e
    setLastChar (Just e)
    lookAhead
      (eof <|> void (satisfy (`elem` emphasisPostChars)))

emphasisPre :: Char -> OrgParser ()
emphasisPre s = try $ do
  lchar <- gets orgStateLastChar
  for_ lchar $ guard . (`elem` emphasisPreChars)
  _ <- char s
  putLastChar Nothing
  notFollowedBy spaceChar

markup ::
  (OrgObjects -> OrgObjects) ->
  Char ->
  Marked OrgParser (F OrgObjects)
markup f c = mark [c] $
  try $ do
    emphasisPre c
    f
      <<$>> withMContext
        (emphasisPost c)
        (plainMarkupContext standardSet)

rawMarkup ::
  (Text -> OrgObjects) ->
  Char ->
  Marked OrgParser (F OrgObjects)
rawMarkup f d = mark [d] $
  try $ do
    emphasisPre d
    str <-
      withMContext
        (emphasisPost d)
        getInput
    pureF $ f str

code :: Marked OrgParser (F OrgObjects)
code = rawMarkup B.code '~'

verbatim :: Marked OrgParser (F OrgObjects)
verbatim = rawMarkup B.verbatim '='

italic :: Marked OrgParser (F OrgObjects)
italic = markup B.italic '/'

underline :: Marked OrgParser (F OrgObjects)
underline = markup B.underline '_'

bold :: Marked OrgParser (F OrgObjects)
bold = markup B.bold '*'

striketrough :: Marked OrgParser (F OrgObjects)
striketrough = markup B.strikethrough '+'

singleQuoted :: Marked OrgParser (F OrgObjects)
singleQuoted = markup B.singleQuoted '\''

doubleQuoted :: Marked OrgParser (F OrgObjects)
doubleQuoted = markup B.doubleQuoted '"'

-- | An endline character that can be treated as a space, not a line break.
endline :: Marked OrgParser (F OrgObjects)
endline =
  mark' '\n' $
    try $
      newlineAndClear
        *> hspace
        $> pure B.softbreak

-- * Entities and LaTeX fragments

entityOrFragment :: Marked OrgParser (F OrgObjects)
entityOrFragment = mark' '\\' $
  try $ do
    _ <- char '\\'
    entity <|> fragment
  where
    entity :: MonadParser m => m (F OrgObjects)
    entity = try $ do
      name <- choice (map string defaultEntitiesNames)
      void (string "{}") <|> notFollowedBy asciiAlpha
      pureF $ B.entity name

    fragment :: MonadParser m => m (F OrgObjects)
    fragment = try $ do
      name <- someAsciiAlpha
      text <- (name <>) <$> option "" brackets
      pureF $ B.fragment ("\\" <> text)

    brackets :: MonadParser m => m Text
    brackets = try $ do
      open <- satisfy (\c -> c == '{' || c == '[')
      str <- takeWhileP Nothing (\c -> c /= open && c /= '\n')
      close <- char (if open == '{' then '}' else ']')
      pure $ open `T.cons` str `T.snoc` close

mathFragment :: Marked OrgParser (F OrgObjects)
mathFragment = mark' '\\' $
  try $ do
    _ <- char '\\'
    open <- satisfy (\c -> c == '(' || c == '[')
    str <-
      findChars2
        '\\'
        (if open == '(' then ')' else ']')
        (Just "insides of math fragment")
    pureF $
      if open == '('
        then B.inlMath str
        else B.dispMath str

texMathFragment :: Marked OrgParser (F OrgObjects)
texMathFragment = mark' '$' $
  try $ do
    display <|> inline
  where
    display = try $ do
      _ <- string "$$"
      str <-
        findChars2
          '$'
          '$'
          (Just "insides of math fragment")
      pureF $ B.dispMath str

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
      pureF $ B.inlMath str

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

exportSnippet :: Marked OrgParser (F OrgObjects)
exportSnippet = mark' '@' . try $ do
  _ <- string "@@"
  backend <-
    takeWhile1P
      (Just "export snippet backend")
      (\c -> isAsciiAlpha c || isDigit c || c == '-')
  _ <- char ':'
  pure . B.exportSnippet backend
    <$> findChars2 '@' '@' (Just "export snippet contents")

-- * Citations

-- The following code for org-cite citations was adapted and improved upon pandoc's.

citation :: Marked OrgParser (F OrgObjects)
citation =
  mark' '[' $
    B.citation <<$>> withBalancedContext '[' ']' (const True) orgCite

-- | A citation in org-cite style
orgCite :: OrgParser (F Citation)
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
  return $ do
    prefix' <- globalPrefix
    suffix' <- globalSuffix
    refs' <- items
    return
      Citation
        { citationStyle = style,
          citationVariant = variant,
          citationPrefix = toList prefix',
          citationSuffix = toList suffix',
          citationReferences = refs'
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

citeItems :: OrgParser (F [CiteReference])
citeItems = sequence <$> (citeItem `sepBy1'` char ';')
  where
    sepBy1' p sep = (:) <$> p <*> many (try $ sep >> p)

citeItem :: OrgParser (F CiteReference)
citeItem = do
  pref <- option mempty citePrefix
  itemKey <- orgCiteKey
  suff <- option mempty citeSuffix
  return $ do
    pre' <- pref
    suf' <- suff
    return
      CiteReference
        { refId = itemKey,
          refPrefix = toList pre',
          refSuffix = toList suf'
        }

citePrefix :: OrgParser (F OrgObjects)
citePrefix = try $ do
  clearLastChar
  withMContext
    ( mark "@;" $
        try $
          eof <|> void (lookAhead $ oneOf ['@', ';'])
    )
    (plainMarkupContext minimalSet)

citeSuffix :: OrgParser (F OrgObjects)
citeSuffix = try $ do
  clearLastChar
  withMContext
    ( mark ";" $
        try $
          eof <|> void (lookAhead $ single ';')
    )
    (plainMarkupContext minimalSet)

orgCiteKey :: OrgParser Text
orgCiteKey = do
  _ <- char '@'
  takeWhile1P (Just "citation key allowed chars") orgCiteKeyChar

orgCiteKeyChar :: Char -> Bool
orgCiteKeyChar c =
  isAlphaNum c || c
    `elem` [ '.',
             ':',
             '?',
             '!',
             '`',
             '\'',
             '/',
             '*',
             '@',
             '+',
             '|',
             '(',
             ')',
             '{',
             '}',
             '<',
             '>',
             '&',
             '_',
             '^',
             '$',
             '#',
             '%',
             '~',
             '-'
           ]

-- * Inline Babel calls

inlBabel :: Marked OrgParser (F OrgObjects)
inlBabel = mark' 'c' . try $ do
  _ <- string "call_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c `notElem` ['[', ']', '(', ')'])
  header1 <- option "" header
  args <- arguments
  header2 <- option "" header
  pureF $ B.inlBabel name header1 header2 args
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    arguments = withBalancedContext '(' ')' (/= '\n') getInput

-- * Inline source blocks

inlSrc :: Marked OrgParser (F OrgObjects)
inlSrc = mark' 's' . try $ do
  _ <- string "src_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c /= '{' && c /= '[')
  headers <- option "" header
  str <- body
  pureF $ B.inlSrc name headers str
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    body = withBalancedContext '{' '}' (/= '\n') getInput

-- * Line breaks

linebreak :: Marked OrgParser (F OrgObjects)
linebreak =
  mark' '\\' . try $
    pure B.linebreak <$ string "\\\\" <* hspace <* newlineAndClear' <* hspace

-- * Links

angleLink :: Marked OrgParser (F OrgObjects)
angleLink = mark' '<' . try $ do
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
  pureF $ B.uriLink protocol tgt (B.plain $ protocol <> ":" <> tgt)

regularLinkOrImage :: Marked OrgParser (F OrgObjects)
regularLinkOrImage =
  mark "[" . try $
    do
      _ <- string "[["
      str <- linkTarget
      optional linkDescr >>= \case
        Just descr -> pure $ liftA2 B.link (fst <$> linkToTarget str) descr
        Nothing -> do
          _ <- char ']'
          return $ do
            (tgt, alias) <- linkToTarget str
            pure $
              if isImgTarget tgt
                then B.image tgt
                else B.link tgt alias
      <* setLastChar (Just ']')
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

    linkDescr :: OrgParser (F OrgObjects)
    linkDescr = try $ do
      _ <- char '['
      st <- getFullState
      str <- findChars2 ']' ']' (Just "link description")
      parsed <-
        parseFromText st str $
          plainMarkupContext standardSet -- FIXME this is not the right set but... whatever
      return $ do
        (tgt', _) <- linkToTarget str
        if isImgTarget tgt'
          then pure $ B.image tgt'
          else parsed

linkToTarget :: Text -> F (LinkTarget, OrgObjects)
linkToTarget l@(T.stripPrefix "/" -> Just fp) = pure (URILink "file" ("//" <> fp), B.plain l)
linkToTarget l@(T.stripPrefix "./" -> Just fp) = pure (URILink "file" fp, B.plain l)
linkToTarget fp | "../" `T.isPrefixOf` fp = pure (URILink "file" fp, B.plain fp)
linkToTarget l@(T.break (== ':') -> (T.toLower -> protocol, T.uncons -> Just (_, uri))) = do
  formatters <- asksF orgStateLinkFormatters
  case lookup protocol formatters of
    Just f -> linkToTarget (f uri)
    Nothing -> pure (URILink protocol uri, B.plain l)
linkToTarget link = do
  docTargets <- asksF orgStateInternalTargets
  case lookup link docTargets of
    Just (li, alias) -> (InternalLink li,) <$> alias
    Nothing -> pure (UnresolvedLink link, B.plain link)

-- | FIXME This is not exactly how org figures out if a link is an image. But
-- for simplicity I may leave it like this anyway.
isImgTarget :: LinkTarget -> Bool
isImgTarget (URILink protocol rest) = hasImgExtension && (protocol `elem` imgProtocols)
  where
    hasImgExtension = any (`T.isSuffixOf` T.toLower rest) imgExtensions
    imgExtensions = [".jpeg", ".jpg", ".png", ".gif", ".svg"]
    imgProtocols = ["file", "http", "https", "attachment"]
isImgTarget _ = False

-- * Targets and radio targets

target :: Marked OrgParser (F OrgObjects)
target = mark' '<' $ try do
  _ <- string "<<"
  str <- takeWhile1P (Just "dedicated target") (\c -> c /= '<' && c /= '>' && c /= '\n')
  guard (not (isSpace $ T.head str))
  guard (not (isSpace $ T.last str))
  _ <- string ">>"
  descr <-
    fromMaybe (pure $ B.text "No description for this link")
      <$> gets orgStateTargetDescriptionCtx
  uid <- registerTarget str descr
  pureF $ B.target uid

-- * Subscripts and superscripts

suscript :: Marked OrgParser (F OrgObjects)
suscript = mark "_^" $ try do
  lchar <- gets orgStateLastChar
  for_ lchar $ guard . not . isSpace
  start <- satisfy \c -> c == '_' || c == '^'
  contents <- asterisk <|> balanced <|> plain
  if start == '_'
    then pure $ B.subscript <$> contents
    else pure $ B.superscript <$> contents
  where
    asterisk = pure . B.plain . one <$> char '*'

    balanced =
      withBalancedContext '{' '}' (const True) $
        plainMarkupContext minimalSet

    sign = pure <$> option mempty (B.plain . one <$> oneOf ['+', '-'])

    plain =
      liftA2 (<>) sign $
        withMContext plainEnd $
          plainMarkupContext entityOrFragment

    plainEnd :: Marked OrgParser ()
    plainEnd = Marked
      (not . isAlphaNum)
      ["non-alphanum chars"]
      $ try do
        lookAhead $
          eof
            <|> try (some (oneOf [',', '.', '\\']) *> notFollowedBy (satisfy isAlphaNum))
            <|> void (noneOf [',', '.', '\\'])

-- * Macros

-- * Footnote references

footnoteReference :: Marked OrgParser (F OrgObjects)
footnoteReference = mark' '[' $
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
      _ -> do
        lbl' <- maybe popUniqueId pure lbl
        whenJust def \def' ->
          registerFootnote lbl' (B.para mempty <$> def')
        pureF $ B.footnoteRef lbl'

-- * Timestamps

timestamp :: Marked OrgParser (F OrgObjects)
timestamp = mark "<[" $ pure . B.timestamp <$> parseTimestamp

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

statisticCookie :: Marked OrgParser (F OrgObjects)
statisticCookie = mark' '[' $ try do
  _ <- char '['
  res <- Left <$> fra <|> Right <$> pct
  _ <- char ']'
  pureF $ B.statisticCookie res
  where
    fra = try $ liftA2 (,) integer (char '/' *> integer)
    pct = try $ integer <* char '%'
