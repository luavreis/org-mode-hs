{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Org.Parser.Definitions
  ( module Org.Parser.Definitions
  , module Org.Types
  , module Org.Builder
  , module Org.Parser.State
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Debug
  , module Data.Char
  )
where

import Data.Char (isAlphaNum, isAscii, isDigit, isLetter, isPunctuation, isSpace)
import Org.Builder (OrgElements, OrgObjects)
import Org.Parser.State
import Org.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Prelude hiding (State)

type Parser = Parsec Void Text

type MonadParser m = MonadParsec Void Text m

newtype OrgParser a = OrgParser (ReaderT OrgParserEnv (StateT OrgParserState Parser) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadState OrgParserState
    , MonadReader OrgParserEnv
    , MonadPlus
    , MonadFail
    , MonadParsec Void Text
    )

type OrgParseError = ParseErrorBundle Text Void

-- * Last char

setLastChar :: Maybe Char -> OrgParser ()
setLastChar lchar =
  modify (\c -> c {lastChar = lchar <|> c.lastChar})

clearLastChar :: OrgParser ()
clearLastChar = modify (\c -> c {lastChar = Nothing})

putLastChar :: Char -> OrgParser ()
putLastChar lchar = modify (\c -> c {lastChar = Just lchar})

withIndentLevel :: Int -> OrgParser a -> OrgParser a
withIndentLevel i = local \s -> s {indentLevel = i}

-- * State and Environment convenience functions

type FullState = (State Text Void, OrgParserState)

getFullState :: OrgParser FullState
getFullState = liftA2 (,) getParserState get

setFullState :: FullState -> OrgParser ()
setFullState (pS, oS) = setParserState pS >> put oS

getsO :: (OrgOptions -> a) -> OrgParser a
getsO f = asks (f . (.options))

-- * Marked parsers

data Marked m a = Marked
  { marks :: String
  , parser :: m a
  }

instance Functor m => Functor (Marked m) where
  fmap f x@(Marked _ p) = x {parser = fmap f p}

instance Alternative m => Semigroup (Marked m a) where
  Marked s1 p1 <> Marked s2 p2 =
    Marked (s1 ++ s2) (p1 <|> p2)

instance Alternative m => Monoid (Marked m a) where
  mempty = Marked [] empty
  mconcat ms =
    Marked
      (foldMap (.marks) ms)
      (choice $ map (.parser) ms)
  {-# INLINE mconcat #-}
