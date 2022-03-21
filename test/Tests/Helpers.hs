{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, DefaultSignatures, ScopedTypeVariables #-}
-- |

module Tests.Helpers
  ( module Tests.Helpers
  , module Test.Tasty
  ) where
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple
import Text.Megaparsec
import Text.Org.Parser.Definitions (OrgParser, Properties, defaultState, F)
import Text.Org.Builder (Many)
import qualified Text.Show

type OrgParseError = ParseErrorBundle Text Void
newtype PrettyError = PrettyError { unPrettyError :: String }

instance Show PrettyError where
  show = unPrettyError

class PrettyParsable a where
  type Parsed a
  prettyParse :: OrgParser Identity a -> Text -> Either PrettyError (Pretty a)
  prettyPrint :: Pretty a -> String
  prettyEq :: Pretty a -> Pretty a -> Bool
  default prettyEq :: (Eq (Parsed a)) => Pretty a -> Pretty a -> Bool
  prettyEq x y = unPretty x == unPretty y

parse' :: OrgParser Identity a -> Text -> Either OrgParseError a
parse' p = parse (evalStateT p defaultState) ""

parseMany :: OrgParser Identity (F (Many a)) -> Text -> Either OrgParseError [a]
parseMany parser txt =
  parse (runStateT parser defaultState) "" txt
  & second (toList . uncurry (runReader . getAp))

newtype Pretty a = Pretty { unPretty :: Parsed a }

instance PrettyParsable a => Eq (Pretty a) where
  (==) = prettyEq

instance PrettyParsable a => Show (Pretty a) where
  show = prettyPrint

instance (Show a, Eq a) => PrettyParsable (F (Many a)) where
  type Parsed (F (Many a)) = [a]
  prettyParse p = bimap (PrettyError . errorBundlePretty) Pretty . parseMany p
  prettyPrint = toString . pShow . toList . unPretty

instance PrettyParsable Properties where
  type Parsed Properties = Properties
  prettyParse p = bimap (PrettyError . errorBundlePretty) Pretty . parse' p
  prettyPrint = toString . pShow . unPretty

infix 5 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

infix 4 =:
(=:) :: (Eq a, Show a) => TestName -> (a, a) -> TestTree
(=:) name (x, y) = testCase name (x @?= y)

infix 4 ~:
(~:) :: PrettyParsable a
  => TestName
  -> OrgParser Identity a
  -> (Text, Parsed a)
  -> TestTree
(~:) name parser (txt, ref) =
  testCase name $
  case prettyParse parser txt of
    Left e  -> assertFailure $ unPrettyError e
    Right x -> x @?= Pretty ref

infix 4 ~!:
(~!:) :: PrettyParsable a
  => TestName
  -> OrgParser Identity a
  -> Text
  -> TestTree
(~!:) name parser txt =
  testCase name $
  case prettyParse parser txt of
    Left _  -> pure ()
    Right x ->
      assertFailure ("Should not parse, but was parsed as: \n" ++ show x)
