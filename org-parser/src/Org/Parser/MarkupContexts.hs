-- | This module used to define a "subparsing" monad but this was later
-- absorbed into OrgState. Maybe I should move its contents elsewhere.
module Org.Parser.MarkupContexts where

import Data.Text qualified as T
import Org.Parser.Common
import Org.Parser.Definitions
import Data.Set (notMember)

skipManyTill' ::
  forall skip end m.
  MonadParser m =>
  m skip ->
  m end ->
  m (Text, end)
skipManyTill' skip end = try $ do
  o0 <- getOffset
  s0 <- getInput
  (o1, final) <- skipManyTill skip (liftA2 (,) getOffset end)
  pure (T.take (o1 - o0) s0, final)
{-# INLINEABLE skipManyTill' #-}

findSkipping ::
  forall end.
  (Char -> Bool) ->
  OrgParser end ->
  OrgParser (Text, end)
findSkipping skip = skipManyTill' toSkip
  where
    toSkip = anySingle *> takeWhileP Nothing skip
{-# INLINEABLE findSkipping #-}

withContext__ ::
  forall a end skip.
  OrgParser skip ->
  OrgParser end ->
  OrgParser a ->
  OrgParser (a, end, Text)
withContext__ skip end p = try do
  clearLastChar
  st <- getFullState
  (str, final) <- skipManyTill' skip end
  guard (not $ T.null str)
  (,final,str) <$> parseFromText st str p
{-# INLINEABLE withContext__ #-}

withContext_ ::
  forall a end skip.
  OrgParser skip ->
  OrgParser end ->
  OrgParser a ->
  OrgParser (a, end)
withContext_ skip end p =
  withContext__ skip end p
    <&> \ ~(x, y, _) -> (x, y)
{-# INLINEABLE withContext_ #-}

withContext ::
  forall a end skip.
  OrgParser skip ->
  OrgParser end ->
  OrgParser a ->
  OrgParser a
withContext skip end = fmap fst . withContext_ skip end
{-# INLINEABLE withContext #-}

withMContext__ ::
  forall a b.
  (Char -> Bool) ->
  OrgParser b ->
  OrgParser a ->
  OrgParser (a, b, Text)
withMContext__ skip end p = try do
  clearLastChar
  st <- getFullState
  prelim <- takeWhileP Nothing skip
  ((prelim <>) -> str, final) <- skipManyTill' toSkip end
  guard (not $ T.null str)
  (,final,str) <$> parseFromText st str p
  where
    toSkip = anySingle *> takeWhileP Nothing skip
{-# INLINEABLE withMContext__ #-}

withMContext ::
  (Char -> Bool) ->
  OrgParser b ->
  OrgParser a ->
  OrgParser a
withMContext skip end p =
  withMContext__ skip end p
    <&> \ ~(x, _, _) -> x
{-# INLINEABLE withMContext #-}

withBalancedContext ::
  Char ->
  Char ->
  -- | Allowed
  (Char -> Bool) ->
  OrgParser a ->
  OrgParser a
withBalancedContext lchar rchar allowed p = try do
  _ <- char lchar
  let skip :: StateT Int OrgParser ()
      skip = do
        _ <-
          takeWhileP
            (Just "insides of markup")
            (\c -> allowed c && c /= lchar && c /= rchar)
        c <- lookAhead (satisfy allowed) <?> "balanced delimiters"
        when (c == lchar) $ modify (+ 1)
        when (c == rchar) $ modify (subtract 1)
        get >>= \case
          balance | balance < 0 -> fail "unbalaced delimiters"
                  | balance == 0 -> pure ()
                  | otherwise -> void anySingle
      end = try do
        guard . (== 0) =<< get
        _ <- char rchar
        lift $ setLastChar (Just rchar)
  st <- getFullState
  (str, _) <- evalStateT (skipManyTill' skip end) 1
  parseFromText st str p

markupContext ::
  Monoid k =>
  (Text -> k) ->
  Marked OrgParser k ->
  OrgParser k
markupContext f elems = go
  where
    go = try $ do
      let specials :: Set Char = fromList $ getMarks elems
      str <-
        optional $
          takeWhile1P
            Nothing
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
          rest <- go
          pure $ el <> rest
        nextChar = try $ do
          c <- anySingle
          -- traceM $ "parsed char: " ++ show c
          setLastChar (Just c)
          rest <- go
          pure $ f (one c) <> rest
