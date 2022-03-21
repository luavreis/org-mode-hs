-- |

module Text.Org.Parser.OrgObjects where
import Prelude hiding (many, some)
import Relude.Extra (notMember)
import Text.Org.Parser.Definitions
import Text.Org.Parser.Common
import Text.Org.Parser.ElementStarts
import qualified Text.Org.Builder as B
import qualified Data.Text as T

data MarkupContext = MarkupContext
    { endOffset :: Int
    , endOfEndOffset :: Int
    , lastChar :: Maybe Char
    }

type WithMContext m = ReaderT MarkupContext (OrgParser m)

contextEnd :: WithMContext m ()
contextEnd = do
  end <- endOffset <$> ask
  endOfEnd <- endOfEndOffset <$> ask
  here <- getOffset
  guard (here == end)
  void $ takeP (Just "end of context") (endOfEnd - here)

runMContext ::
  MOrgParser m ()
  -> WithMContext m a
  -> OrgParser m a
runMContext end p = do
  flip runReaderT (MarkupContext maxBound maxBound Nothing) $
    withMContext (mapParser lift end) p

withMContext ::
  Marked (WithMContext m) ()
  -> WithMContext m a
  -> WithMContext m a
withMContext end p = do
  pEnd  <- endOffset <$> ask
  let
    marks = getMarks end
    find' = do
      str <- optional $ takeWhile1P
             (Just $ "markup context without " ++ show (toList marks))
             (`notMember` marks)
      (guard . (< pEnd)) =<< getOffset
      withLastChar (T.last <$> str) $
        lookAhead (endPosOf_ $ getParser end)
          <|> anySingle *> find'
  (endP', endP) <- endPosOf (try find')
  lchar <- lastChar  <$> ask
  lift $ runReaderT p (MarkupContext endP endP' lchar)

withLastChar ::
  Maybe Char -- ^ End parser for new context
  -> WithMContext m a -- ^ New context
  -> WithMContext m a
withLastChar lchar ctx = do
  plchar <- lastChar <$> ask
  endP <- endOffset <$> ask
  endP' <- endOfEndOffset <$> ask
  lift $ runReaderT ctx (MarkupContext endP endP' (lchar <|> plchar))

markupContext :: Monoid k
  => (Text -> k)
  -> Marked (WithMContext m) k
  -> WithMContext m k
markupContext f elems = try $ do
  let specials = getMarks elems
  str <- optional $ takeWhile1P
         (Just $ "chars not in " ++ show (toList specials))
         (`notMember` specials)
  let self = maybe mempty f str
  (self <>) <$> withLastChar (T.last <$> str)
    (finishSelf <|> anotherEl <|> nextChar)
  where
    finishSelf = do
      contextEnd
      pure mempty
    anotherEl = do
      el <- getParser elems
      rest <- markupContext f elems
      pure $ el <> rest
    nextChar = do
      el <- f . T.singleton <$> anySingle
      rest <- markupContext f elems
      pure $ el <> rest

plainMarkupContext :: Marked (WithMContext m) (F OrgInlines) -> WithMContext m (F OrgInlines)
plainMarkupContext = markupContext (pure . B.plain)

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
emphasisPostChars = " ,.-\t\n:!?;'\")}["

emphasisPost :: Char -> Marked (WithMContext m) ()
emphasisPost e = mark [e] $ do
  lchar <- lastChar <$> ask
  for_ lchar $ guard . not . isSpace
  _ <- char e
  lookAhead
    (contextEnd <|> void (satisfy (`elem` emphasisPostChars)))

guardPreChar :: WithMContext m ()
guardPreChar = do
  lchar <- lastChar <$> ask
  for_ lchar $ guard . (`elem` emphasisPreChars)

markup :: Monad m
  => (OrgInlines -> OrgInlines)
  -> Char
  -> Marked (WithMContext m) (F OrgInlines)
markup f c = mark [c] $ try $ do
  guardPreChar
  _ <- char c
  notFollowedBy spaceChar
  f <<$>> withMContext (emphasisPost c)
    (plainMarkupContext standardP)

rawMarkup ::
  (Text -> OrgInlines)
  -> Char
  -> Marked (WithMContext m) (F OrgInlines)
rawMarkup f d = mark [d] $ try $ do
  guardPreChar
  _ <- char d
  notFollowedBy spaceChar
  str <- withMContext (emphasisPost d)
         (markupContext id (mark [d] empty))
  pureF $ f str

code :: Marked (WithMContext m) (F OrgInlines)
code = rawMarkup B.code '~'

verbatim :: Marked (WithMContext m) (F OrgInlines)
verbatim = rawMarkup B.verbatim '='

italic :: Monad m => Marked (WithMContext m) (F OrgInlines)
italic = markup B.italic '/'

underline :: Monad m => Marked (WithMContext m) (F OrgInlines)
underline = markup B.underline '_'

bold :: Monad m => Marked (WithMContext m) (F OrgInlines)
bold = markup B.bold '*'

striketrough :: Monad m => Marked (WithMContext m) (F OrgInlines)
striketrough = markup B.strikethrough '+'

linebreak :: Marked (WithMContext m) (F OrgInlines)
linebreak =  mark "\n" $ try $
  pure B.linebreak <$ string "\\\\" <* hspace <* newline <* hspace

-- | An endline character that can be treated as a space, not a line break.
endline :: Monad m => Marked (WithMContext m) (F OrgInlines)
endline = mark "\n" $ lift . try $
  newline
  *> hspace
  *> notFollowedBy endOfBlock
  $> pure B.softbreak

minimalP :: Monad m => Marked (WithMContext m) (F OrgInlines)
minimalP =
  mconcat [ endline
          , code
          , verbatim
          , italic
          , underline
          , bold
          , striketrough
          ]

standardP :: Monad m => Marked (WithMContext m) (F OrgInlines)
standardP =
  mconcat [ minimalP
          ]

-- * Citations

citation :: Monad m => Marked (WithMContext m) (F OrgInlines)
citation = fmap B.cite <$> orgCite

-- | A citation in org-cite style
orgCite :: Monad m => Marked (WithMContext m) (F OrgCitation)
orgCite = mark "[" $ lift $ try $ do
  _ <- string "[cite"
  (style, variant) <- citeStyle
  _ <- char ':'
  hspace
  _ <- optional newlineInside
  globalPrefix <- option (pure mempty) (try (citePrefix <* char ';'))
  items <- citeItems
  globalSuffix <- option (pure mempty) (try (char ';' *> citeSuffix))
  hspace
  _ <- optional newlineInside
  _ <- char ']'
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

citePrefix :: Monad m => InlineParser m
citePrefix =
   runMContext
     (mark "@;" $ try $ hspace *> lookAhead (oneOf ['@', ';']) $> ())
     (plainMarkupContext standardP)

citeSuffix :: Monad m => InlineParser m
citeSuffix =
   runMContext
     (mark ";" $ try $ hspace *> lookAhead (oneOf [';']) $> ())
     (plainMarkupContext standardP)

orgCiteKey :: OrgParser m Text
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
