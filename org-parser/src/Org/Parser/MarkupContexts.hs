-- | This module used to define a "subparsing" monad but this was later
-- absorbed into OrgState. Maybe I should move its contents elsewhere.
module Org.Parser.MarkupContexts where

import Data.Text qualified as T
import Org.Parser.Common
import Org.Parser.Definitions

withMContext__ ::
  forall a b.
  Marked OrgParser b ->
  OrgParser a ->
  OrgParser (a, b, Text)
withMContext__ end p = try do
  clearLastChar
  st <- getFullState
  (str, final) <- findMarked end
  guard (not $ T.null str)
  -- traceM $ "parsing in substring: " ++ show str
  -- traceM $ "with last char: " ++ show ctx
  -- traceM $ "with last char after substring: " ++ show ctx'
  (,final,str) <$> parseFromText st str p

withMContext_ ::
  forall a b.
  Marked OrgParser b ->
  OrgParser a ->
  OrgParser (a, b)
withMContext_ end p =
  withMContext__ end p
    <&> \ ~(x, y, _) -> (x, y)

withMContext ::
  Marked OrgParser b ->
  OrgParser a ->
  OrgParser a
withMContext end = fmap fst . withMContext_ end

withBalancedContext ::
  Char ->
  Char ->
  -- | Allowed
  (Char -> Bool) ->
  OrgParser a ->
  OrgParser a
withBalancedContext lchar rchar allowed p = try do
  _ <- char lchar
  let find' :: StateT Int OrgParser Text
      find' = do
        str <-
          takeWhileP
            (Just "insides of markup")
            (\c -> allowed c && c /= lchar && c /= rchar)
        c <- satisfy allowed <?> "balanced delimiters"
        when (c == lchar) $ modify (+ 1)
        when (c == rchar) $ modify (subtract 1)
        balance <- get
        if
            | balance == 0 -> pure str
            | balance > 0 -> (T.snoc str c <>) <$> find'
            | otherwise -> fail "unbalaced delimiters"
  st <- getFullState
  str <- evalStateT find' 1
  -- traceM $ "balanced parsing in substring: " ++ show str
  parseFromText st str p
    <* setLastChar (Just rchar)

markupContext ::
  Monoid k =>
  (Text -> k) ->
  Marked OrgParser k ->
  OrgParser k
markupContext f elems = go
  where
    go = try $ do
      let specials = getMarks elems
      str <-
        optional $
          takeWhile1P
            ( Just $
                "insides of markup (not "
                  <> getMDescription elems
                  <> ")"
            )
            (not . specials)
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
          rest <- go
          pure $ el <> rest
        nextChar = try $ do
          c <- anySingle
          -- traceM $ "parsed char: " ++ show c
          setLastChar (Just c)
          rest <- go
          pure $ f (one c) <> rest
