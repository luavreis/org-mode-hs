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

import Prelude hiding (State)
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

type OrgParseError = ParseErrorBundle Text Void

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

registerTarget :: Text -> F OrgInlines -> OrgParser Text
registerTarget name alias = do
  targets <- gets orgStateInternalTargets
  uid <- popUniqueId
  updateState \s -> s { orgStateInternalTargets = insert name (uid, alias) targets }
  pure uid

registerAnchorTarget :: Text -> Text -> F OrgInlines -> OrgParser ()
registerAnchorTarget name anchor alias = do
  targets <- gets orgStateInternalTargets
  updateState \s -> s { orgStateInternalTargets = insert name (anchor, alias) targets }

clearPendingAffiliated :: OrgParser ()
clearPendingAffiliated = modify (\s -> s { orgStatePendingAffiliated = mempty })

withAffiliated :: (Affiliated -> a) -> OrgParser a
withAffiliated f = f <$> gets orgStatePendingAffiliated
                   <* clearPendingAffiliated


-- * Last char

setLastChar :: Maybe Char -> OrgParser ()
setLastChar lchar =
  modify (\c -> c { orgStateLastChar = lchar <|> orgStateLastChar c })

clearLastChar :: OrgParser ()
clearLastChar = modify (\c -> c { orgStateLastChar = Nothing })

putLastChar :: Maybe Char -> OrgParser ()
putLastChar lchar = modify (\c -> c { orgStateLastChar = lchar })


-- * State and Future convenience functions

type FullState = (State Text Void, OrgParserState)

getState :: OrgParser OrgParserState
getState = get

getFullState :: OrgParser FullState
getFullState = liftA2 (,) getParserState getState

setFullState :: FullState -> OrgParser ()
setFullState (pS, oS) = setParserState pS >> put oS

updateState ::
  (OrgParserState -> OrgParserState) ->
  OrgParser ()
updateState = modify

askF :: F OrgParserState
askF = Ap ask

getsO :: MonadState OrgParserState m => (OrgOptions -> a) -> m a
getsO f = f <$> gets orgStateOptions

asksF :: (OrgParserState -> a) -> F a
asksF f = Ap $ asks f

pureF :: Monad m => a -> m (F a)
pureF = pure . pure


-- * Marked parsers

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
