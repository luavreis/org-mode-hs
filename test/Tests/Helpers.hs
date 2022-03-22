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
import Text.Org.Parser.Definitions
import Text.Org.Builder (Many)
import Text.Org.Parser.MarkupContexts

type OrgParseError = ParseErrorBundle Text Void

class Parsable m a where
  type Parsed m a
  parse' :: m a -> Text -> Either OrgParseError (Parsed m a)

instance Parsable OrgParser (F a) where
  type Parsed OrgParser (F a) = (F a, OrgParserState)
  parse' p = parse (runStateT p defaultState) ""

instance Parsable OrgParser Properties where
  type Parsed OrgParser Properties = Properties
  parse' p = parse (evalStateT p defaultState) ""

instance Parsable WithMContext (F a) where
  type Parsed WithMContext (F a) = (F a, OrgParserState)
  parse' p = parse (runStateT (evalStateT p (MarkupContext Nothing)) defaultState) ""

instance Parsable (Marked WithMContext) (F a) where
  type Parsed (Marked WithMContext) (F a) = (F a, OrgParserState)
  parse' p = parse (runStateT (evalStateT (getParser p) (MarkupContext Nothing)) defaultState) ""

class PrettyFormable a where
  type PrettyForm a
  prettyForm :: a -> PrettyForm a

instance PrettyFormable Properties where
  type PrettyForm Properties = Properties
  prettyForm = id

instance PrettyFormable (Many a) where
  type PrettyForm (Many a) = [a]
  prettyForm = toList

instance PrettyFormable a => PrettyFormable (F a, OrgParserState) where
  type PrettyForm (F a, OrgParserState) = PrettyForm a
  prettyForm = prettyForm . uncurry (runReader . getAp)

prettyParse :: (Parsable m a, PrettyFormable (Parsed m a), Show (PrettyForm (Parsed m a))) => m a -> Text -> IO ()
prettyParse parser txt =
  case parse' parser txt of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> pPrint $ prettyForm x

infix 5 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

infix 4 =:
(=:) :: (Eq a, Show a) => TestName -> (a, a) -> TestTree
(=:) name (x, y) = testCase name (x @?= y)

infix 4 ~:
(~:) :: (Parsable m a, PrettyFormable (Parsed m a), Eq (PrettyForm (Parsed m a)), Show (PrettyForm (Parsed m a)))
  => TestName
  -> m a
  -> (Text, PrettyForm (Parsed m a))
  -> TestTree
(~:) name parser (txt, ref) =
  testCase name $
  case parse' parser txt of
    Left e  -> assertFailure $ errorBundlePretty e
    Right x -> prettyForm x @?= ref

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
