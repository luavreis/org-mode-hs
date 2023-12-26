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

withPos :: OrgParser OrgObjectD -> OrgParser OrgObjects
withPos p = do
  s <- getOffset
  r <- p
  e <- getOffset
  return $ object s e r

-- * Sets of objects

minimalSet :: Marked (OrgParser OrgObjects)
minimalSet =
  withPos
    <$> mconcat
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

standardSet :: Marked (OrgParser OrgObjects)
standardSet =
  (minimalSet <>)
    $ withPos
    <$> mconcat
      [ regularLink
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
plainMarkupContext :: Marked (OrgParser OrgObjects) -> OrgParser OrgObjects
plainMarkupContext = markupContext (\s e t -> object s e (Plain t))

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
  (OrgObjects -> OrgObjectD) ->
  Char ->
  Marked (OrgParser OrgObjectD)
markup f c = Marked [c] $ try do
  emphasisPre c
  st <- getFullState
  s <- anySingle
  (t, _) <- skipManyTill' (emphasisSkip c) (emphasisPost c)
  f <$> parseFromText st (T.cons s t) (plainMarkupContext standardSet)

rawMarkup ::
  (Text -> OrgObjectData Org ObjIx) ->
  Char ->
  Marked (OrgParser OrgObjectD)
rawMarkup f d = Marked [d] $ try do
  emphasisPre d
  f . fst <$> skipManyTill' (emphasisSkip d) (emphasisPost d)

-- | Parse a code object.
code :: Marked (OrgParser OrgObjectD)
code = rawMarkup Code '~'

-- | Parse a verbatim object.
verbatim :: Marked (OrgParser OrgObjectD)
verbatim = rawMarkup Verbatim '='

-- | Parse an italic object.
italic :: Marked (OrgParser OrgObjectD)
italic = markup Italic '/'

-- | Parse an underline object.
underline :: Marked (OrgParser OrgObjectD)
underline = markup Underline '_'

-- | Parse a bold object.
bold :: Marked (OrgParser OrgObjectD)
bold = markup Bold '*'

-- | Parse a strikethrough object.
strikethrough :: Marked (OrgParser OrgObjectD)
strikethrough = markup Strikethrough '+'

-- | Parse a single-quoted object.
singleQuoted :: Marked (OrgParser OrgObjectD)
singleQuoted = markup (Quoted SingleQuote) '\''

-- | Parse a double-quoted object.
doubleQuoted :: Marked (OrgParser OrgObjectD)
doubleQuoted = markup (Quoted DoubleQuote) '"'

-- TODO why is this parsed? can't it live inside plain??

-- | An endline character that can be treated as a space, not a line break.
endline :: Marked (OrgParser OrgObjectD)
endline = Marked "\n" $ try $ newlineAndClear *> hspace $> Plain "\n"

-- * Entities and LaTeX fragments

-- | Parse an entity object.
entity :: Marked (OrgParser OrgObjectD)
entity = Marked "\\" $ try do
  _ <- char '\\'
  name <- choice (map string defaultEntitiesNames)
  void (string "{}") <|> notFollowedBy asciiAlpha
  pure $ Entity name

-- | Parse a LaTeX fragment object.
latexFragment :: Marked (OrgParser OrgObjectD)
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
        $ if inline
          then LaTeXFragment InlMathFragment str
          else LaTeXFragment DispMathFragment str

    rawFragment = try do
      name <- someAsciiAlpha
      text <- (name <>) <$> option "" brackets
      pure $ LaTeXFragment RawFragment ("\\" <> text)

    brackets = try $ do
      open <- satisfy (\c -> c == '{' || c == '[')
      let close = if open == '{' then '}' else ']'
      str <- takeWhileP Nothing (\c -> c /= close && c /= '\n')
      _ <- char close
      pure $ open `T.cons` str `T.snoc` close

-- | Parse a TeX math fragment object.
texMathFragment :: Marked (OrgParser OrgObjectD)
texMathFragment = Marked "$" $ try $ display <|> inline
  where
    display = try $ do
      _ <- string "$$"
      (str, _) <-
        findSkipping
          (/= '$')
          (string "$$")
      pure $ LaTeXFragment DispMathFragment str

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
      pure $ LaTeXFragment InlMathFragment str

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
exportSnippet :: Marked (OrgParser OrgObjectD)
exportSnippet = Marked "@" $ try do
  _ <- string "@@"
  backend <-
    takeWhile1P
      (Just "export snippet backend")
      (\c -> isAsciiAlpha c || isDigit c || c == '-')
  _ <- char ':'
  ExportSnippet backend . fst <$> findSkipping (/= '@') (string "@@")

-- * Citations

-- The following code for org-cite citations was adapted and improved upon pandoc's.

-- | Parse a citation object.
citation :: Marked (OrgParser OrgObjectD)
citation = Marked "[" do
  Cite <$> withBalancedContext '[' ']' (const True) orgCite

-- | A citation in org-cite style
orgCite :: OrgParser (Citation OrgObjects)
orgCite = try $ do
  _ <- string "cite"
  (style, variant) <- citeStyle
  _ <- char ':'
  space
  globalPrefix <- optional (try (citeSuffix <* char ';'))
  items <- citeItems
  globalSuffix <- optional (try (char ';' *> citePrefix))
  space
  eof
  return
    Citation
      { style = style
      , variant = variant
      , prefix = globalPrefix
      , suffix = globalSuffix
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
  pref <- optional citePrefix
  itemKey <- orgCiteKey
  suff <- optional citeSuffix
  return
    CiteReference
      { id = itemKey
      , prefix = pref
      , suffix = suff
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
inlBabel :: Marked (OrgParser OrgObjectD)
inlBabel = Marked "c" $ try do
  _ <- string "call_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c `notElem` ['[', ']', '(', ')'])
  header1 <- option "" header
  args <- arguments
  header2 <- option "" header
  return $ InlBabelCall $ BabelCall name header1 header2 args
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    arguments = withBalancedContext '(' ')' (/= '\n') getInput

-- * Inline source blocks

-- | Parse an inline source object.
inlSrc :: Marked (OrgParser OrgObjectD)
inlSrc = Marked "s" $ try do
  _ <- string "src_"
  name <-
    takeWhile1P
      (Just "babel call name")
      (\c -> not (isSpace c) && c /= '{' && c /= '[')
  headers <- option "" header
  Src name headers <$> body
  where
    header = withBalancedContext '[' ']' (/= '\n') getInput
    body = withBalancedContext '{' '}' (/= '\n') getInput

-- * Line breaks

-- | Parse a linebreak object.
linebreak :: Marked (OrgParser OrgObjectD)
linebreak = Marked "\\" $ try do
  LineBreak <$ string "\\\\" <* blankline' <* clearLastChar

-- * Links

-- | Parse a angle link object.
angleLink :: Marked (OrgParser OrgObjectD)
angleLink = Marked "<" $ try do
  _ <- char '<'
  s <- getOffset
  protocol <- manyAsciiAlpha
  _ <- char ':'
  tgt <- fix $ \search -> do
    partial <-
      takeWhile1P
        (Just "angle link target")
        (\c -> c /= '\n' && c /= '>')
    char '>' $> partial <|> newline *> hspace *> ((T.stripEnd partial <>) <$> search)
  e <- getOffset
  return $ Link (URILink protocol tgt) (object s (e - 1) $ Plain $ protocol <> ":" <> tgt)

-- | Parse a regular link object.
regularLink :: Marked (OrgParser OrgObjectD)
regularLink = Marked "[" $ try do
  _ <- string "[["
  str <- linkTarget
  descr <- linkDescr <|> char ']' $> mempty
  putLastChar ']'
  return $ Link (linkToTarget str) descr
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
target :: Marked (OrgParser OrgObjectD)
target = Marked "<" $ try do
  _ <- string "<<"
  str <- takeWhile1P (Just "dedicated target") (\c -> c /= '<' && c /= '>' && c /= '\n')
  guard (not (isSpace $ T.head str))
  guard (not (isSpace $ T.last str))
  _ <- string ">>"
  return $ Target str

-- * Subscripts and superscripts

-- | Parse a subscript or a superscript object.
suscript :: Marked (OrgParser OrgObjectD)
suscript = Marked "_^" $ try do
  lchar <- gets (.lastChar)
  for_ lchar $ guard . not . isSpace
  start <- satisfy \c -> c == '_' || c == '^'
  contents <- asterisk <|> balanced <|> plain
  pure
    $ if start == '_'
      then Subscript contents
      else Superscript contents
  where
    asterisk = withPos $ Plain . T.singleton <$> char '*'

    balanced =
      withBalancedContext '{' '}' (const True)
        $ plainMarkupContext minimalSet

    sign = option mempty (withPos $ Plain . one <$> oneOf ['+', '-'])

    plain =
      liftA2 (<>) sign
        $ withMContext (const True) isAlphaNum plainEnd
        $ plainMarkupContext (withPos <$> entity <> latexFragment)

    plainEnd :: OrgParser ()
    plainEnd = try do
      lookAhead
        $ eof
        <|> try (some (oneOf [',', '.', '\\']) *> notFollowedBy (satisfy isAlphaNum))
        <|> void (noneOf [',', '.', '\\'])

-- * Macros

-- | Parse a macro object.
macro :: Marked (OrgParser OrgObjectD)
macro = Marked "{" $ try do
  _ <- string "{{{"
  _ <- lookAhead $ satisfy isAsciiAlpha
  key <- takeWhile1P Nothing allowedKeyChar
  args <-
    (string "}}}" $> []) <|> do
      _ <- char '('
      t <- fst <$> findSkipping (/= ')') (string ")}}}")
      return $ T.split (== ',') t
  return $ Macro key args
  where
    allowedKeyChar c = isAsciiAlpha c || isDigit c || c == '-' || c == '_'

-- * Footnote references

-- | Parse a footnote reference object.
footnoteReference :: Marked (OrgParser OrgObjectD)
footnoteReference = Marked "[" $ withBalancedContext '[' ']' (const True) do
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
      return $ FootnoteRef (FootnoteRefLabel lbl')
    (_, Just def') ->
      return $ FootnoteRef (FootnoteRefDef lbl def')

-- * Timestamps

-- | Parse a timestamp object.
timestamp :: Marked (OrgParser OrgObjectD)
timestamp = Marked "<[" $ Timestamp <$> parseTimestamp

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
statisticCookie :: Marked (OrgParser OrgObjectD)
statisticCookie = Marked "[" $ try do
  _ <- char '['
  res <- Left <$> fra <|> Right <$> pct
  _ <- char ']'
  return $ StatisticCookie res
  where
    fra = try $ liftA2 (,) integer (char '/' *> integer)
    pct = try $ integer <* char '%'
