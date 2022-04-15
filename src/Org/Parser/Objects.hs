-- |

module Org.Parser.Objects where

import Prelude hiding (many, some)
import Org.Parser.Common
import Org.Parser.Definitions
import Org.Parser.MarkupContexts
import Org.Data.Entities (defaultEntitiesNames)
import Relude.Extra
import qualified Org.Builder as B
import qualified Data.Text as T

-- * Sets of objects

minimalSet :: Marked OrgParser (F OrgInlines)
minimalSet =
  mconcat [ endline
          , code
          , verbatim
          , italic
          , underline
          , bold
          , striketrough
          , entityOrFragment
          , mathFragment
          , texMathFragment
          , singleQuoted
          , doubleQuoted
          ]

standardSet ::  Marked OrgParser (F OrgInlines)
standardSet =
  mconcat [ minimalSet -- TODO optimize with ordering? in minimal too
          , citation
          , timestamp
          , exportSnippet
          , inlBabel
          , inlSrc
          , linebreak
          , angleLink
          , regularLinkOrImage
          ]

plainMarkupContext :: Marked OrgParser (F OrgInlines) -> OrgParser (F OrgInlines)
plainMarkupContext = markupContext (pure . B.plain)

newlineAndClear :: OrgParser Char
newlineAndClear = newline <* clearLastChar

emphasisPreChars :: String
emphasisPreChars = "-\t ('\"{\8203"

emphasisPostChars :: String
emphasisPostChars = " ,.-\t\n:!?;'\")}[\8203"

emphasisPost :: Char -> Marked OrgParser ()
emphasisPost e = mark [e] $ try $ do
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
  (OrgInlines -> OrgInlines)
  -> Char
  -> Marked OrgParser (F OrgInlines)
markup f c = mark [c] $ try $ do
  emphasisPre c
  f <<$>> withMContext (emphasisPost c)
    (plainMarkupContext standardSet)

rawMarkup ::
  (Text -> OrgInlines)
  -> Char
  -> Marked OrgParser (F OrgInlines)
rawMarkup f d = mark [d] $ try $ do
  emphasisPre d
  str <- withMContext (emphasisPost d)
         getInput
  pureF $ f str

code :: Marked OrgParser (F OrgInlines)
code = rawMarkup B.code '~'

verbatim :: Marked OrgParser (F OrgInlines)
verbatim = rawMarkup B.verbatim '='

italic :: Marked OrgParser (F OrgInlines)
italic = markup B.italic '/'

underline :: Marked OrgParser (F OrgInlines)
underline = markup B.underline '_'

bold :: Marked OrgParser (F OrgInlines)
bold = markup B.bold '*'

striketrough :: Marked OrgParser (F OrgInlines)
striketrough = markup B.strikethrough '+'

singleQuoted :: Marked OrgParser (F OrgInlines)
singleQuoted = markup B.singleQuoted '\''

doubleQuoted :: Marked OrgParser (F OrgInlines)
doubleQuoted = markup B.doubleQuoted '"'

-- | An endline character that can be treated as a space, not a line break.
endline ::  Marked OrgParser (F OrgInlines)
endline = mark "\n" $ try $
  newlineAndClear
  *> hspace
  $> pure B.softbreak

-- * Entities and LaTeX fragments

entityOrFragment :: Marked OrgParser (F OrgInlines)
entityOrFragment = mark "\\" $ try $ do
  _ <- char '\\'
  entity <|> fragment
  where
    entity :: MonadParser m => m (F OrgInlines)
    entity = try $ do
      name <- choice (map string defaultEntitiesNames)
      void (string "{}") <|> notFollowedBy asciiAlpha
      pureF $ B.entity name

    fragment :: MonadParser m => m (F OrgInlines)
    fragment = try $ do
      name <- someAsciiAlpha
      text <- (name <>) <$> option "" brackets
      pureF $ B.fragment text

    brackets :: MonadParser m => m Text
    brackets = try $ do
      open <- satisfy (\c -> c == '{' || c == '[')
      str <- takeWhileP Nothing (\c -> c /= open && c /= '\n')
      close <- char (if open == '{' then '}' else ']')
      pure $ open `T.cons` str `T.snoc` close

mathFragment :: Marked OrgParser (F OrgInlines)
mathFragment = mark "\\" $ try $ do
  _ <- char '\\'
  open <- satisfy (\c -> c == '(' || c == '[')
  str <- findChars2 '\\' (if open == '(' then ')' else ']')
         (Just "insides of math fragment")
  pureF $ if open == '('
    then B.inlMath str
    else B.dispMath str

texMathFragment :: Marked OrgParser (F OrgInlines)
texMathFragment = mark "$" $ try $ do
  display <|> inline
  where
    display = try $ do
      _ <- string "$$"
      str <- findChars2 '$' '$'
             (Just "insides of math fragment")
      pureF $ B.dispMath str

    post = do
      _ <- char '$'
      void . lookAhead $
        satisfy (\x -> isPunctuation x || isSpace x || x == '"')

    inline = try $ do
      lchar <- gets orgStateLastChar
      for_ lchar $ guard . (/= '$')
      str <- singleChar <|> moreChars
      pureF $ B.inlMath str

    moreChars = try $ do
      str <- takeWhile1P (Just "inside of inline math") (/= '$')
      guard $ border1 (T.head str) && border2 (T.last str)
      post
      pure str

    singleChar = try $ do
      c <- satisfy (\x -> not (isSpace x) && x `notMember` allowedCharsSet)
      post
      pure $ T.singleton c

    allowedCharsSet :: Set Char
    allowedCharsSet = fromList ['.',',','?',';','"']

    border1 c = not (isSpace c) && c `notElem` (".,;$" :: String)
    border2 c = not (isSpace c) && c `notElem` (".,$" :: String)

-- * Export snippets

exportSnippet :: Marked OrgParser (F OrgInlines)
exportSnippet = mark "@" . try $ do
  _ <- string "@@"
  backend <- takeWhile1P (Just "export snippet backend")
             (\c -> isAsciiAlpha c || isDigit c || c == '-')
  _ <- char ':'
  pure . B.exportSnippet backend <$>
    findChars2 '@' '@' (Just "export snippet contents")

-- * Citations

citation :: Marked OrgParser (F OrgInlines)
citation = mark "[" $
  B.citation <<$>> withBalancedContext '[' ']' mempty orgCite

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
    return Citation
      { citationStyle = style
      , citationVariant = variant
      , citationPrefix = toList prefix'
      , citationSuffix = toList suffix'
      , citationReferences = refs'
      }

citeStyle :: OrgParser (Tokens Text, Tokens Text)
citeStyle = do
  sty <- option "" $ try style
  vars <- option "" $ try variants
  return (sty, vars)
  where
    style = char '/' *>
            takeWhileP (Just "alphaNum, '_' or '-' characters")
            (\c -> isAlphaNum c || c == '_' || c == '-')
    variants = char '/' *>
               takeWhileP (Just "alphaNum, '_', '-' or '/' characters")
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
    return CiteReference
      { refId     = itemKey
      , refPrefix = toList pre'
      , refSuffix = toList suf'
      }

citePrefix :: OrgParser (F OrgInlines)
citePrefix = try $ do
  clearLastChar
  withMContext
    (mark "@;" $ try $
      eof <|> void (lookAhead $ oneOf ['@', ';']))
    (plainMarkupContext minimalSet)

citeSuffix :: OrgParser (F OrgInlines)
citeSuffix = try $ do
  clearLastChar
  withMContext
    (mark ";" $ try $
      eof <|> void (lookAhead $ single ';'))
    (plainMarkupContext minimalSet)

orgCiteKey :: OrgParser Text
orgCiteKey = do
  _ <- char '@'
  takeWhile1P (Just "citation key allowed chars") orgCiteKeyChar

orgCiteKeyChar :: Char -> Bool
orgCiteKeyChar c =
  isAlphaNum c || c `elem` ['.',':','?','!','`','\'','/','*','@','+','|',
                            '(',')','{','}','<','>','&','_','^','$','#',
                            '%','~','-']


-- * Inline Babel calls

inlBabel :: Marked OrgParser (F OrgInlines)
inlBabel = mark "c" . try $ do
  _ <- string "call_"
  name <- takeWhile1P (Just "babel call name")
          (\c -> not (isSpace c) && c `notElem` ['[', ']', '(', ')'])
  header1 <- option "" header
  args <- arguments
  header2 <- option "" header
  pureF $ B.inlBabel name header1 header2 args
  where
    header = withBalancedContext '[' ']' (All . (/= '\n')) getInput
    arguments = withBalancedContext '(' ')' (All . (/= '\n')) getInput


-- * Inline source blocks

inlSrc :: Marked OrgParser (F OrgInlines)
inlSrc = mark "s" . try $ do
  _ <- string "src_"
  name <- takeWhile1P (Just "babel call name")
          (\c -> not (isSpace c) && c /= '{' && c /= '[')
  headers <- option "" header
  str <- body
  pureF $ B.inlSrc name headers str
  where
    header = withBalancedContext '[' ']' (All . (/= '\n')) getInput
    body = withBalancedContext '{' '}' (All . (/= '\n')) getInput


-- * Line breaks

linebreak :: Marked OrgParser (F OrgInlines)
linebreak =  mark "\\" . try $
  pure B.linebreak <$ string "\\\\" <* hspace <* newlineAndClear <* hspace


-- * Links

angleLink :: Marked OrgParser (F OrgInlines)
angleLink = mark "<" . try $ do
  _ <- char '<'
  protocol <- manyAsciiAlpha
  _ <- char ':'
  tgt <- fix $ \search -> do
    partial <- takeWhile1P (Just "angle link target")
               (\c -> c /= '\n' && c /= '>')
    char '>' $> partial
      <|> newline *> hspace *> ((T.stripEnd partial <>) <$> search)
  pureF $ B.uriLink protocol tgt (B.plain $ protocol <> ":" <> tgt)

regularLinkOrImage :: Marked OrgParser (F OrgInlines)
regularLinkOrImage = mark "[" . try $ do
  _ <- string "[["
  str <- linkTarget
  optional linkDescr >>= \case
    Just descr -> pure $ liftA2 B.link (fst <$> linkToTarget str) descr
    Nothing -> do
      _ <- char ']'
      return $ do
        (target, alias) <- linkToTarget str
        pure $ if isImgTarget target
               then B.image target
               else B.link target alias
  <* setLastChar (Just ']')
  where
    linkTarget :: MonadParser m => m Text
    linkTarget = fix $ \rest -> do
      partial <- takeWhileP (Just "link target")
                  (\c -> c /= ']' && c /= '[' && c /= '\\' && c /= '\n')
      oneOf ['[', ']'] $> partial
        <|> char '\\' *> liftA2 T.cons (option '\\' $ oneOf ['[', ']']) rest
        <|> newline *> hspace *> ((T.stripEnd partial `T.snoc` ' ' <>) <$> rest)

    linkDescr :: OrgParser (F OrgInlines)
    linkDescr = try $ do
      _ <- char '['
      st <- getFullState
      str <- findChars2 ']' ']' (Just "link description")
      parsed <- parseFromText st str $
                plainMarkupContext standardSet -- FIXME this is not the right set but... whatever
      return $ do
        (tgt', _) <- linkToTarget str
        if isImgTarget tgt'
        then pure $ B.image tgt'
        else parsed

linkToTarget :: Text -> F (LinkTarget, OrgInlines)
linkToTarget l@(T.stripPrefix  "/" -> Just fp) = pure (URILink "file" ("//" <> fp), B.plain l)
linkToTarget l@(T.stripPrefix "./" -> Just fp) = pure (URILink "file" fp, B.plain l)
linkToTarget fp | "../" `T.isPrefixOf` fp      = pure (URILink "file" fp, B.plain fp)
linkToTarget l@(second T.uncons . T.break (== ':') -> (protocol, Just (_, uri))) = do
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
   hasImgExtension = any (\x -> T.cons '.' x `T.isSuffixOf` rest) imgExtensions
   imgExtensions = [ ".jpeg", ".jpg", ".png", ".gif", ".svg" ]
   imgProtocols = [ "file", "http", "https", "attachment" ]
isImgTarget _ = False


-- * Macros



-- * Timestamps

timestamp :: Marked OrgParser (F OrgInlines)
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
      year  <- number 4 <* char '-'
      month <- number 2 <* char '-'
      day   <- number 2
      dayName <- optional $ do
        hspace1
        takeWhile1P (Just "dayname characters") isLetter
      pure (year, month, day, dayName)

    parseTime :: OrgParser Time
    parseTime = do
      hour   <- (number 2 <|> number 1) <* char ':'
      minute <- number 2
      pure (hour, minute)

    repeaterMark = tsmark ["++", ".+", "+"]

    warningMark = tsmark ["--", "-"]

    tsmark :: [Text] -> OrgParser TimestampMark
    tsmark marks = do
      mtype <- (,,) <$> choice (map string marks)
      mtype <$> integer <*> oneOf ['h', 'm', 'd', 'w']
