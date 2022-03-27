{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}
-- |

module Tests.Helpers
  ( module Tests.Helpers
  , module Test.Tasty
  ) where
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple
import Text.Megaparsec
import Org.Parser.Definitions
import Org.Builder (Many)
import Org.Parser.MarkupContexts
import qualified Text.Show

type OrgParseError = ParseErrorBundle Text Void

-- | This class is mainly used for the tests cases.
-- @Parsed m a@ is the "monad-stripped" version of parse
-- tree with which we can compare in the test cases.
class Parsable m a where
  type Parsed m a
  parse' :: m a -> Text -> Either OrgParseError (Parsed m a)

applyFuture :: (F a, OrgParserState) -> a
applyFuture = uncurry (runReader . getAp)

instance Parsable OrgParser (F a) where
  type Parsed OrgParser (F a) = a
  parse' p = second applyFuture .
    parse (runStateT p defaultState) ""

instance Parsable WithMContext (F a) where
  type Parsed WithMContext (F a) = a
  parse' p = parse' (evalStateT p defaultMCtx)

instance Parsable (Marked WithMContext) (F a) where
  type Parsed (Marked WithMContext) (F a) = a
  parse' p = parse' (getParser p)

instance Parsable OrgParser Properties where
  type Parsed OrgParser Properties = Properties
  parse' p = parse (evalStateT p defaultState) ""

instance PrettyFormable Properties where
  type PrettyForm Properties = Properties
  prettyForm = id

instance Parsable OrgParser OrgDocument where
  type Parsed OrgParser OrgDocument = OrgDocument
  parse' p = parse (evalStateT p defaultState) ""

instance PrettyFormable OrgDocument where
  type PrettyForm OrgDocument = OrgDocument
  prettyForm = id

class PrettyFormable a where
  type PrettyForm a
  prettyForm :: a -> PrettyForm a

instance PrettyFormable (Many a) where
  type PrettyForm (Many a) = [a]
  prettyForm = toList

prettyParse :: (Parsable m a, PrettyFormable (Parsed m a), Show (PrettyForm (Parsed m a))) => m a -> Text -> IO ()
prettyParse parser txt =
  case parse' parser txt of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> pPrint $ prettyForm x

newtype Pretty a = Pretty { unPretty :: a }

instance Eq a => Eq (Pretty a) where
  x == y = unPretty x == unPretty y

instance Show a => Show (Pretty a) where
  show = toString . pShow . unPretty

infix 1 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

infix 4 =:
(=:) :: (Eq a, Show a) => TestName -> (a, a) -> TestTree
(=:) name (x, y) = testCase name (x @?= y)

infix 4 ~:
(~:) :: (Parsable m a, PrettyFormable (Parsed m a), Eq (Parsed m a), Show (Parsed m a))
  => TestName
  -> m a
  -> [(Text, Parsed m a)]
  -> TestTree
(~:) name parser cases =
  testGroup name $ flip (`zipWith` [0..]) cases $ \ (i :: Int) (txt, ref) ->
    testCase (name <> " " <> show i) $
      case parse' parser txt of
        Left e  -> assertFailure $ errorBundlePretty e
        Right x -> Pretty x @?= Pretty ref

-- infix 4 ~!:
-- (~!:) :: PrettyParsable a
--   => TestName
--   -> OrgParser a
--   -> Text
--   -> TestTree
-- (~!:) name parser txt =
--   testCase name $
--   case prettyParse parser txt of
--     Left _  -> pure ()
--     Right x ->
--       assertFailure ("Should not parse, but was parsed as: \n" ++ show x)
