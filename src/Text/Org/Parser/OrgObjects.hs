-- |

module Text.Org.Parser.OrgObjects where
import Prelude hiding (many, some)
import Text.Org.Parser.Definitions
import Text.Org.Parser.Common
import Text.Org.Parser.ElementStarts
import qualified Data.Text as T
import qualified Text.Org.Builder as B

type InlineParser m = OrgParser m (F OrgInlines)

-- | This is used for inline elements that can have line breaks
-- inside, like citations and angle links. The priority is given to
-- new blocks (hence @notFollowedBy endOfBlock@) and initial
-- whitespace is ignored.
newlineInside :: OrgParser m ()
newlineInside = try $
  newline
  *> notFollowedBy endOfBlock
  *> hspace

emphasisPreChars :: String
emphasisPreChars = "-\t ('\"{"

emphasisPostChars :: String
emphasisPostChars = "-\t\n .,:!?;'\")}["

takeWithSpecial :: forall m. (Monad m) =>
  (Text -> OrgInlines) -- ^ "Default" constructor
  -> [Char] -- ^ Special characters
  -> [InlineParser m] -- ^ Special parsers
  -> [InlineParser m] -- ^ Pre-char parsers
  -> Bool -- ^ Forbids space before end?
  -> Maybe Char -- ^ Last char
  -> OrgParser m () -- ^ End parser
  -> InlineParser m
takeWithSpecial f chars sP pP fS lC end = try $ do
  str <- optional $ takeWhile1P (Just $ "none of " <> show chars) (`notElem` chars)
  (maybe mempty B.str str <>) <<$>> withLast (T.last <$> str <|> lC)
  where
    withLast :: Maybe Char -> InlineParser m
    withLast last' = do
      (for_ last' (\c -> guard (not (fS && isSpace c)))
       *> end
       $> pure mempty) <|> do
        el <- choice sP
              <|> do for_ last' $ guard . (`elem` emphasisPreChars)
                     choice pP
              <|> do c <- anySingle
                     pureF (f $ T.singleton c)
        rest <- takeWithSpecial f chars sP pP fS last' end
        return $ do
          el' <- el
          rest' <- rest
          pure $ el' <> rest'


emphasisPost :: Char -> OrgParser m ()
emphasisPost e = try $ do
  char e
  notFollowedBy (satisfy (`notElem` emphasisPostChars))

rawMarkup ::
  (Text -> OrgInlines)
  -> Char
  -> InlineParser m
rawMarkup f d = try $ do
  char d
  notFollowedBy spaceChar
  str <- takeWhile1P (Just "raw markup") (\c -> c /= '\n' && c /= d)
  guard (not . isSpace $ T.last str)
  emphasisPost d
  pureF $ f str

code :: InlineParser m
code = rawMarkup B.code '~'

verbatim :: InlineParser m
verbatim = rawMarkup B.verbatim '='

markup :: Monad m
  => (OrgInlines -> OrgInlines)
  -> Char
  -> InlineParser m
markup f c = try $ do
  char c
  notFollowedBy spaceChar
  f <<$>>
    takeWithSpecial
      B.str
      standardSpecial
      standardP
      minimalPCP
      True
      Nothing
      (emphasisPost c)

emph :: Monad m => InlineParser m
emph = markup B.emph '/'

underline :: Monad m => InlineParser m
underline = markup B.underline '_'

bold :: Monad m => InlineParser m
bold = markup B.bold '*'

striketrough :: Monad m => InlineParser m
striketrough = markup B.strikethrough '+'

linebreak :: InlineParser m
linebreak = try $ pure B.linebreak <$ string "\\\\" <* hspace <* newline <* hspace

-- | An endline character that can be treated as a space, not a line break.
endline :: InlineParser m
endline = try $
  newline
  *> hspace
  *> notFollowedBy endOfBlock
  $> pure B.softbreak

-- treat these as potentially non-text when parsing inline:
minimalSpecial :: [Char]
minimalSpecial =
  ['\n', '\'', '$', '"', '*', '/'
  , '+', '=', '\\', '^', '_', '~'
  ]

minimalP :: [InlineParser m]
minimalP =
  [ endline
  ]

minimalPCP :: Monad m => [InlineParser m]
minimalPCP =
  [ code
  , verbatim
  , emph
  , underline
  , bold
  , striketrough
  ]

standardSpecial :: [Char]
standardSpecial = minimalSpecial ++
  [ '@', '[', '{'
  ]

standardP :: Monad m => [InlineParser m]
standardP = minimalP ++
  [ timestamp
  , cite
  , linebreak
  ]

plain :: Monad m => InlineParser m
plain =
  takeWithSpecial
    B.str
    standardSpecial
    standardP
    minimalPCP
    False
    Nothing
    (try $ space *> endOfBlock)


-- * Citations

cite :: Monad m => InlineParser m
cite = fmap B.cite <$> orgCite

-- | A citation in org-cite style
orgCite :: Monad m => OrgParser m (F OrgCitation)
orgCite = try $ do
  string "[cite"
  (style, variant) <- citeStyle
  char ':'
  hspace
  optional newlineInside
  globalPrefix <- option (pure mempty) (try (citePrefix <* char ';'))
  items <- citeItems
  globalSuffix <- option (pure mempty) (try (char ';' *> citeSuffix))
  hspace
  optional newlineInside
  char ']'
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

citeStyle :: OrgParser m (Text, Text)
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

citeItems :: Monad m => OrgParser m (F [OrgReference])
citeItems = sequence <$> citeItem `sepBy1` char ';'

citeItem :: Monad m => OrgParser m (F OrgReference)
citeItem = do
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

citeBracketed :: Monad m => InlineParser m
citeBracketed = try $ do
  char '['
  (\e -> B.str "[" <> e <> B.str "]") <<$>>
    takeWithSpecial
      B.str
      (minimalSpecial ++ [';', '@', '[', ']'])
      (citeBracketed : minimalP)
      minimalPCP
      False
      (Just '[')
      (void $ lookAhead (oneOf [']', ';', '@']))
    <* char ']'

citePrefix :: Monad m => InlineParser m
citePrefix =
  takeWithSpecial
    B.str
    (minimalSpecial ++ [';', '@', '[', ']'])
    (citeBracketed : minimalP)
    minimalPCP
    False
    Nothing
    (void . lookAhead $ oneOf [';', '@', ']'])

citeSuffix :: Monad m => InlineParser m
citeSuffix =
  takeWithSpecial
    B.str
    (minimalSpecial ++ [';', '[', ']'])
    (citeBracketed : minimalP)
    minimalPCP
    False
    Nothing
    (void . lookAhead $ oneOf [';', ']'])

orgCiteKey :: OrgParser m Text
orgCiteKey = do
  char '@'
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
parseTimestamp :: OrgParser m TimestampData
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
      char (fst delims)
      date <- parseDate
      time <- optional . try $ do
        hspace1
        startTime <- parseTime
        endTime <- optional . try $ char '-' *> parseTime
        pure (startTime, endTime)
      rods <- many (try $ hspace1 *> repeaterOrDelay)
      hspace
      char (snd delims)
      pure (date, time, rods)

    parseDate :: OrgParser m Date
    parseDate = do
      year  <- number 4 <* char '-'
      month <- number 2 <* char '-'
      day   <- number 2
      dayName <- optional $ do
        hspace1
        takeWhile1P (Just "dayname characters") isLetter
      pure (year, month, day, dayName)

    parseTime :: OrgParser m Time
    parseTime = do
      hour   <- (number 2 <|> number 1) <* char ':'
      minute <- number 2
      pure (hour, minute)

    repeaterOrDelay :: OrgParser m RepeaterOrDelay
    repeaterOrDelay = do
      mtype <- Repeater <$> choice (map string ["++", ".+", "+"]) <|>
               Delay <$> choice (map string ["--", "-"])
      mtype <$> integer <*> oneOf ['h', 'm', 'd', 'w']
