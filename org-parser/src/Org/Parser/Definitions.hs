{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Org.Parser.Definitions
  ( module Org.Parser.Definitions,
    module Org.Types,
    module Org.Builder,
    module Org.Parser.State,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Debug,
    module Data.Char,
  )
where

import Data.Char (isAlphaNum, isAscii, isDigit, isLetter, isPunctuation, isSpace)
import Data.Set qualified as Set
import Org.Builder (OrgElements, OrgObjects)
import Org.Parser.State
import Org.Types
import Relude.Extra (insert, member, notMember)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Prelude hiding (State)

type Parser = ParsecT Void Text Identity

type MonadParser m = MonadParsec Void Text m

type OrgParser = StateT OrgParserState Parser

type OrgParseError = ParseErrorBundle Text Void

setSrcLineNum :: Int -> OrgParser ()
setSrcLineNum n = updateState $ \s ->
  s {orgStateSrcLineNumber = n}

incSrcLineNum :: Int -> OrgParser ()
incSrcLineNum n = updateState $ \s ->
  s {orgStateSrcLineNumber = orgStateSrcLineNumber s + n}

getSrcLineNum :: OrgParser Int
getSrcLineNum = gets orgStateSrcLineNumber

registerKeyword :: F (KeywordKey, KeywordValue) -> OrgParser ()
registerKeyword kw =
  updateState \s ->
    s
      { orgStateKeywords =
          kw : orgStateKeywords s
      }

registerAffiliated :: F (KeywordKey, KeywordValue) -> OrgParser ()
registerAffiliated kw =
  updateState \s ->
    s
      { orgStatePendingAffiliated =
          kw : orgStatePendingAffiliated s
      }

registerFootnote :: Text -> F OrgElements -> OrgParser ()
registerFootnote k v =
  updateState \s ->
    s
      { orgStateFootnotes =
          insert k v (orgStateFootnotes s)
      }

clearPendingAffiliated :: OrgParser ()
clearPendingAffiliated = modify (\s -> s {orgStatePendingAffiliated = []})

withAffiliated :: (Affiliated -> a) -> OrgParser (F a)
withAffiliated f = do
  affs <- sequence <$> gets orgStatePendingAffiliated
  (f . keywordsFromList <$> affs)
    <$ clearPendingAffiliated

-- * Last char

setLastChar :: Maybe Char -> OrgParser ()
setLastChar lchar =
  modify (\c -> c {orgStateLastChar = lchar <|> orgStateLastChar c})

clearLastChar :: OrgParser ()
clearLastChar = modify (\c -> c {orgStateLastChar = Nothing})

putLastChar :: Maybe Char -> OrgParser ()
putLastChar lchar = modify (\c -> c {orgStateLastChar = lchar})

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
  { getMarks :: String,
    getParser :: m a
  }

instance Functor m => Functor (Marked m) where
  fmap f x@(Marked _ p) = x {getParser = fmap f p}

instance Alternative m => Semigroup (Marked m a) where
  Marked s1 p1 <> Marked s2 p2 =
    Marked (s1 ++ s2) (p1 <|> p2)

instance Alternative m => Monoid (Marked m a) where
  mempty = Marked [] empty
  mconcat ms =
    Marked
      (foldMap getMarks ms)
      (choice $ map getParser ms)
  {-# INLINE mconcat #-}
