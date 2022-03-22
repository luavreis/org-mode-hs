-- |

module Text.Org.Parser.OrgObjects where
import Prelude hiding (many, some)
import Text.Org.Parser.Common
import Text.Org.Parser.Definitions
import Text.Org.Parser.ElementStarts
import Text.Org.Parser.MarkupContexts
import qualified Text.Org.Builder as B

plainMarkupContext :: Marked WithMContext (F OrgInlines) -> WithMContext (F OrgInlines)
plainMarkupContext = markupContext (pure . B.plain)

-- inside, like citations and angle links. The priority is given to
-- new blocks (hence @notFollowedBy endOfBlock@) and initial
-- whitespace is ignored.
newlineInside :: OrgParser ()
newlineInside = try $
  newline
  *> notFollowedBy endOfBlock
  *> hspace

emphasisPreChars :: String
emphasisPreChars = "-\t ('\"{"

emphasisPostChars :: String
emphasisPostChars = " ,.-\t\n:!?;'\")}["

emphasisPost :: Char -> Marked WithMContext ()
emphasisPost e = mark [e] $ do
  lchar <- lastChar <$> get
  for_ lchar $ guard . not . isSpace
  _ <- char e
  setLastChar (Just e)
  lookAhead
    (eof <|> void (satisfy (`elem` emphasisPostChars)))

emphasisPre :: Char -> WithMContext ()
emphasisPre s = do
  lchar <- lastChar <$> get
  for_ lchar $ guard . (`elem` emphasisPreChars)
  _ <- char s
  setLastChar (Just s)
  notFollowedBy spaceChar

markup ::
  (OrgInlines -> OrgInlines)
  -> Char
  -> Marked WithMContext (F OrgInlines)
markup f c = mark [c] $ try $ do
  emphasisPre c
  f <<$>> withMContext (emphasisPost c)
    (plainMarkupContext standardP)

rawMarkup ::
  (Text -> OrgInlines)
  -> Char
  -> Marked WithMContext (F OrgInlines)
rawMarkup f d = mark [d] $ try $ do
  emphasisPre d
  str <- withMContext (emphasisPost d)
         (markupContext id (mark [d] empty))
  pureF $ f str

code :: Marked WithMContext (F OrgInlines)
code = rawMarkup B.code '~'

verbatim :: Marked WithMContext (F OrgInlines)
verbatim = rawMarkup B.verbatim '='

italic :: Marked WithMContext (F OrgInlines)
italic = markup B.italic '/'

underline :: Marked WithMContext (F OrgInlines)
underline = markup B.underline '_'

bold :: Marked WithMContext (F OrgInlines)
bold = markup B.bold '*'

striketrough :: Marked WithMContext (F OrgInlines)
striketrough = markup B.strikethrough '+'

linebreak :: Marked WithMContext (F OrgInlines)
linebreak =  mark "\n" $ try $
  pure B.linebreak <$ string "\\\\" <* hspace <* newline <* hspace

-- | An endline character that can be treated as a space, not a line break.
endline ::  Marked WithMContext (F OrgInlines)
endline = mark "\n" $ lift . try $
  newline
  *> hspace
  *> notFollowedBy endOfBlock
  $> pure B.softbreak

minimalP :: Marked WithMContext (F OrgInlines)
minimalP =
  mconcat [ endline
          , code
          , verbatim
          , italic
          , underline
          , bold
          , striketrough
          ]

standardP ::  Marked WithMContext (F OrgInlines)
standardP =
  mconcat [ minimalP
          ]

-- * Citations

citation :: Marked WithMContext (F OrgInlines)
citation = mark "[" $
  B.cite <<$>> withBalancedContext ('[', ']') orgCite

-- | A citation in org-cite style
orgCite :: WithMContext (F OrgCitation)
orgCite = lift $ do
  _ <- string "cite"
  (style, variant) <- citeStyle
  _ <- char ':'
  hspace
  _ <- optional newlineInside
  globalPrefix <- option (pure mempty) (try (citePrefix <* char ';'))
  items <- citeItems
  globalSuffix <- option (pure mempty) (try (char ';' *> citeSuffix))
  hspace
  _ <- optional newlineInside
  return $ do
    prefix' <- globalPrefix
    suffix' <- globalSuffix
    refs' <- items
    return OrgCitation
      { citationStyle = style
      , citationVariant = variant
      , citationPrefix = toList prefix'
      , citationSuffix = toList suffix'
      , citationReferences = refs'
      }

citeStyle :: OrgParser (Text, Text)
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

citeItems :: OrgParser (F [OrgReference])
citeItems = sequence <$> citeItem `sepBy1` char ';'

citeItem :: OrgParser (F OrgReference)
citeItem = try $ do
  pref <- citePrefix
  itemKey <- orgCiteKey
  suff <- citeSuffix
  return $ do
    pre' <- pref
    suf' <- suff
    return OrgReference
      { refId     = itemKey
      , refPrefix = toList pre'
      , refSuffix = toList suf'
      }

citePrefix ::  InlineParser m
citePrefix =
   runMContext
     (mark "@;" $ try $
      hspace
      *> (eof <|> void (lookAhead $ oneOf ['@', ';'])))
     (plainMarkupContext standardP)

citeSuffix :: InlineParser m
citeSuffix =
   runMContext
     (mark ";" $ try $
      hspace
      *> (eof <|> void (lookAhead $ single ';')))
     (plainMarkupContext standardP)

orgCiteKey :: OrgParser Text
orgCiteKey = do
  _ <- char '@'
  takeWhile1P (Just "citation key allowed chars") orgCiteKeyChar

orgCiteKeyChar :: Char -> Bool
orgCiteKeyChar c =
  isAlphaNum c || c `elem` ['.',':','?','!','`','\'','/','*','@','+','|',
                            '(',')','{','}','<','>','&','_','^','$','#',
                            '%','~','-']


-- * Timestamps

timestamp :: InlineParser m
timestamp = pure . B.timestamp <$> parseTimestamp

-- | Read a timestamp.
parseTimestamp :: OrgParser TimestampData
parseTimestamp = try $ do
  openChar <- lookAhead $ satisfy (\c -> c == '<' || c == '[')
  let isActive = openChar == '<'
      closeChar = if isActive then '>' else ']'
      delims = (openChar, closeChar)
  (d1, t1, r1) <- component delims
  optional (try $ string "--" *> component delims)
    >>= \case
    Just (d2, t2, r2) ->
      pure $ TimestampRange isActive (d1, fst <$> t1, r1) (d2, fst <$> t2, r2)
    Nothing -> case t1 of
      Just (t1', Just t1'') ->
        pure $ TimestampRange isActive (d1, Just t1', r1) (d1, Just t1'', r1)
      _ ->
        pure $ TimestampData isActive (d1, fst <$> t1, r1)
  where
    component delims = do
      _ <- char (fst delims)
      date <- parseDate
      time <- optional . try $ do
        hspace1
        startTime <- parseTime
        endTime <- optional . try $ char '-' *> parseTime
        pure (startTime, endTime)
      rods <- many (try $ hspace1 *> repeaterOrDelay)
      hspace
      _ <- char (snd delims)
      pure (date, time, rods)

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

    repeaterOrDelay :: OrgParser RepeaterOrDelay
    repeaterOrDelay = do
      mtype <- Repeater <$> choice (map string ["++", ".+", "+"]) <|>
               Delay <$> choice (map string ["--", "-"])
      mtype <$> integer <*> oneOf ['h', 'm', 'd', 'w']
