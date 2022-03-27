-- |

module Org.Parser.MarkupContexts where

import Org.Parser.Common
import Org.Parser.Definitions
import Data.Set (notMember)
import qualified Data.Text as T

newtype MarkupContext = MarkupContext
    { lastChar :: Maybe Char
    }
    deriving (Show)

defaultMCtx :: MarkupContext
defaultMCtx = MarkupContext Nothing

type WithMContext = StateT MarkupContext OrgParser

setLastChar :: Maybe Char -> WithMContext ()
setLastChar lchar = modify (\c -> c { lastChar = lchar <|> lastChar c })

clearLastChar :: WithMContext ()
clearLastChar = modify (\c -> c { lastChar = Nothing })

ctxChar :: Char -> WithMContext Char
ctxChar c = char c <* setLastChar (Just c)

putLastChar :: Maybe Char -> WithMContext ()
putLastChar lchar = modify (\c -> c { lastChar = lchar })

runMContext_ ::
  Marked OrgParser b
  -> WithMContext a
  -> OrgParser (a, b)
runMContext_ end p = do
  flip evalStateT defaultMCtx $
    withMContext_ (mapParser lift end) p

runMContext ::
  Marked OrgParser ()
  -> WithMContext a
  -> OrgParser a
runMContext end = fmap fst . runMContext_ end

withMContext_ :: forall a b.
  Marked WithMContext b
  -> WithMContext a
  -> WithMContext (a, b)
withMContext_ end p = do
  let
    marks = getMarks end
    find' :: WithMContext (Text, b)
    find' = do
      str <- takeWhileP
             (Just $ "insides of mcontext (chars outside  " ++
              show (toList marks) ++ ")")
             (`notMember` marks)
      setLastChar (snd <$> T.unsnoc str)
      ((str,) <$> getParser end <?> "end of mcontext")
        <|> (do c <- anySingle <?> "insides of mcontext (single)"
                first (T.snoc str c <>) <$> find')
  st <- getParserState
  ctx <- get
  ((str, final), ctx') <- lift $ runStateT (try find') ctx
  guard (not $ T.null str)
  -- traceM $ "parsing in substring: " ++ show str
  -- traceM $ "with last char: " ++ show ctx
  -- traceM $ "with last char after substring: " ++ show ctx'
  (, final) <$> parseFromText (Just st) str p
    <* put ctx'

withMContext ::
  Marked WithMContext b
  -> WithMContext a
  -> WithMContext a
withMContext end = fmap fst . withMContext_ end

withBalancedContext ::
  Char
  -> Char
  -> WithMContext a
  -> WithMContext a
withBalancedContext lchar rchar p = try $ do
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
  -- traceM $ "balanced parsing in substring: " ++ show str
  setLastChar (snd <$> T.unsnoc str)
  parseFromText (Just st) str p
    <* setLastChar (Just rchar)

markupContext :: Monoid k
  => (Text -> k)
  -> Marked WithMContext k
  -> WithMContext k
markupContext f elems = try $ do
  let specials = getMarks elems
  str <- optional $ takeWhile1P
         (Just $ "insides of markup (chars not in "
          ++ show (toList specials) ++ ")")
         (`notMember` specials)
  -- traceM $ "consumed: " ++ show str
  let self = maybe mempty f str
  setLastChar (T.last <$> str)
  (self <>) <$> (finishSelf <|> anotherEl <|> nextChar)
  where
    finishSelf = try $ do
      eof
      pure mempty
    anotherEl = try $ do
      el <- getParser elems
      rest <- markupContext f elems
      pure $ el <> rest
    nextChar = try $ do
      c <- anySingle
      -- traceM $ "parsed char: " ++ show c
      setLastChar (Just c)
      rest <- markupContext f elems
      pure $ f (T.singleton c) <> rest
