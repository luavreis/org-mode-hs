{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
-- |

module Org.Parser.Definitions
  ( module Org.Parser.Definitions
  , module Org.Types
  , module Org.Builder
  , module Org.Parser.State
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Debug
  , module Data.Char
  ) where

import Org.Types
import Org.Parser.State
import Org.Builder (OrgElements, OrgInlines)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Char (isSpace, isPunctuation, isAlphaNum, isLetter, isDigit, isAscii)
import Relude.Extra (insert)

type Parser = ParsecT Void Text Identity

type MonadParser m = MonadParsec Void Text m

type OrgParser = StateT OrgParserState Parser

getState :: OrgParser OrgParserState
getState = get

updateState
  :: (OrgParserState -> OrgParserState)
  -> OrgParser ()
updateState = modify

popUniqueId :: OrgParser Text
popUniqueId = do
  ids <- gets orgStateIdStack
  case ids of
    (x:xs) -> x <$ updateState (\s -> s {orgStateIdStack = xs})
    [] -> error "something's wrong. out of unique ids"

setSrcLineNum :: Int -> OrgParser ()
setSrcLineNum n = updateState $ \s ->
  s { orgStateSrcLineNumber = n }

incSrcLineNum :: Int -> OrgParser ()
incSrcLineNum n = updateState $ \s ->
  s { orgStateSrcLineNumber = orgStateSrcLineNumber s + n }

getSrcLineNum :: OrgParser Int
getSrcLineNum = gets orgStateSrcLineNumber

registerTarget :: Text -> InternalLinkType -> F OrgInlines -> OrgParser Text
registerTarget name kind alias = do
  targets <- gets orgStateInternalTargets
  uid <- popUniqueId
  updateState \s -> s { orgStateInternalTargets = insert name (uid, kind, alias) targets }
  pure uid

withAffiliated :: (Affiliated -> a) -> OrgParser a
withAffiliated f = f <$> gets orgStatePendingAffiliated

askF :: F OrgParserState
askF = Ap ask

asksO :: (OrgOptions -> a) -> OrgParser a
asksO f = f <$> gets orgStateOptions

asksF :: (OrgParserState -> a) -> F a
asksF f = Ap $ asks f

pureF :: Monad m => a -> m (F a)
pureF = pure . pure

data Marked m a = Marked
  { getMarks :: Set Char -- TODO use (Pred {toPred :: Char -> Bool})
  , getParser :: m a
  }

mapParser :: (m1 a -> m2 a) -> Marked m1 a -> Marked m2 a
mapParser f (Marked marks parser) = Marked marks (f parser)

mark :: String -> m a -> Marked m a
mark = Marked . fromList

unmarked :: m a -> Marked m a
unmarked = Marked mempty

instance Functor m => Functor (Marked m) where
  fmap f x@(Marked _ p) = x { getParser = fmap f p }

instance Alternative m => Semigroup (Marked m a) where
  Marked s1 p1 <> Marked s2 p2 =
    Marked (s1 <> s2) (p1 <|> p2)

instance Alternative m => Monoid (Marked m a) where
  mempty = Marked mempty empty
  mconcat xs = Marked (foldMap getMarks xs) (choice $ map getParser xs)
