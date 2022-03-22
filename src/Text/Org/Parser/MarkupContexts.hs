-- |

module Text.Org.Parser.MarkupContexts where
import Data.Set (notMember)
import Text.Org.Parser.Common
import Text.Org.Parser.Definitions
import qualified Data.Text as T

newtype MarkupContext = MarkupContext
    { lastChar :: Maybe Char
    }
    deriving (Show)

type WithMContext = StateT MarkupContext OrgParser

setLastChar :: Maybe Char -> WithMContext ()
setLastChar lchar = modify (\c -> c { lastChar = lchar <|> lastChar c })

runMContext ::
  MOrgParser m ()
  -> WithMContext a
  -> OrgParser a
runMContext end p = do
  flip evalStateT (MarkupContext Nothing) $
    withMContext (mapParser lift end) p

withMContext ::
  Marked WithMContext ()
  -> WithMContext a
  -> WithMContext a
withMContext end p = do
  let
    marks = getMarks end
    find' = do
      str <- takeWhileP
             (Just "insides of markup")
             (`notMember` marks)
      setLastChar (snd <$> T.unsnoc str)
      (getParser end <?> "end of markup") $> str
        <|> (do
          c <- anySingle <?> "insides of markup (single)"
          (T.snoc str c <>) <$> find')
  st <- getParserState
  ctx <- get
  (str, ctx') <- lift $ runStateT (try find') ctx
  parseFromText (Just st) p str
    <* put ctx'

withBalancedContext ::
  (Char, Char)
  -> WithMContext a
  -> WithMContext a
withBalancedContext (lchar, rchar) p = try $ do
  _ <- char lchar
  let
    find' :: StateT Int WithMContext Text
    find' = do
      str <- takeWhileP
             (Just "insides of markup")
             (\c -> c /= lchar && c /= rchar)
      c <- anySingle <?> "balanced delimiters";
      when (c == lchar) $ modify (+ 1)
      when (c == rchar) $ modify (subtract 1)
      balance <- get
      if | balance == 0 -> pure str
         | balance > 0  -> (T.snoc str c <>) <$> find'
         | otherwise    -> fail "unbalaced delimiters"
  st <- getParserState
  str <- evalStateT find' 1
  parseFromText (Just st) p str
    <* setLastChar (Just rchar)

markupContext :: Monoid k
  => (Text -> k)
  -> Marked WithMContext k
  -> WithMContext k
markupContext f elems = try $ do
  let specials = getMarks elems
  str <- optional $ takeWhile1P
         (Just $ "chars not in " ++ show (toList specials))
         (`notMember` specials)
  let self = maybe mempty f str
  setLastChar (T.last <$> str)
  (self <>) <$> (finishSelf <|> anotherEl <|> nextChar)
  where
    finishSelf = do
      eof
      pure mempty
    anotherEl = do
      el <- getParser elems
      rest <- markupContext f elems
      pure $ el <> rest
    nextChar = do
      el <- f . T.singleton <$> anySingle
      rest <- markupContext f elems
      pure $ el <> rest
