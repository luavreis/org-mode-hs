{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Helpers
  ( module Tests.Helpers
  , module Test.Tasty
  )
where

import Data.TreeDiff
import Org.Builder (Many)
import Org.Parser
import Org.Parser.Objects (Marked (..))
import Org.Types
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof)
import Text.Megaparsec.Error (errorBundlePretty)

{- | This class is mainly used for the tests cases.
@Parsed m a@ is the "monad-stripped" version of parse
tree with which we can compare in the test cases.
-}
class Parsable m where
  parse' :: m a -> Text -> Either OrgParseError a

instance Parsable OrgParser where
  parse' p = parseOrg defaultOrgOptions (p <* eof) ""

instance Parsable (Marked OrgParser) where
  parse' p = parse' (getParser p)

instance PrettyFormable Properties where
  type PrettyForm Properties = Properties
  prettyForm = id

instance PrettyFormable OrgDocument where
  type PrettyForm OrgDocument = OrgDocument
  prettyForm = id

class PrettyFormable a where
  type PrettyForm a
  prettyForm :: a -> PrettyForm a

instance PrettyFormable (Many a) where
  type PrettyForm (Many a) = [a]
  prettyForm = toList

instance PrettyFormable OrgElementData where
  type PrettyForm OrgElementData = OrgElementData
  prettyForm = id

prettyParse :: (Parsable m, PrettyFormable a, ToExpr (PrettyForm a)) => m a -> Text -> IO ()
prettyParse parser txt =
  case parse' parser txt of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> print $ ansiWlBgExpr $ toExpr $ prettyForm x

infix 1 =?>

(=?>) :: a -> b -> (a, Either () b)
x =?> y = (x, Right y)

infix 1 =!>

(=!>) :: a -> () -> (a, Either () c)
x =!> y = (x, Left y)

infix 4 =:

(=:) :: (Eq a, Show a) => TestName -> (a, a) -> TestTree
(=:) name (x, y) = testCase name (x @?= y)

infix 4 ~:

(~:) ::
  HasCallStack =>
  (Parsable m, PrettyFormable a, Eq a, ToExpr (PrettyForm a)) =>
  TestName ->
  m a ->
  [(Text, Either () a)] ->
  TestTree
(~:) name parser cases =
  testGroup name $
    flip (`zipWith` [1 ..]) cases $ \(i :: Int) (txt, ref) ->
      testCase (name <> " " <> show i) $
        case parse' parser txt of
          Left e
            | isRight ref -> assertFailure $ errorBundlePretty e
            | otherwise -> pure ()
          Right got
            | Right reference <- ref ->
                unless (got == reference) $
                  assertFailure (diffExpr got reference)
            | otherwise ->
                assertFailure $
                  "Should not parse, but parsed as:\n" <> renderExpr got
  where
    renderExpr :: (PrettyFormable a, ToExpr (PrettyForm a)) => a -> String
    renderExpr = show . ansiWlBgExpr . toExpr . prettyForm
    diffExpr :: (PrettyFormable a, ToExpr (PrettyForm a)) => a -> a -> String
    diffExpr a b = show $ ansiWlBgEditExpr $ ediff (toExpr $ prettyForm a) (toExpr $ prettyForm b)

deriving instance (ToExpr OrgDocument)
deriving instance (ToExpr KeywordValue)
deriving instance (ToExpr OrgObject)
deriving instance (ToExpr QuoteType)
deriving instance (ToExpr TimestampData)
deriving instance (ToExpr FragmentType)
deriving instance (ToExpr FootnoteRefData)
deriving instance (ToExpr Citation)
deriving instance (ToExpr CiteReference)
deriving instance (ToExpr BabelCall)
deriving instance (ToExpr LinkTarget)
deriving instance (ToExpr OrgElement)
deriving instance (ToExpr OrgElementData)
deriving instance (ToExpr GreaterBlockType)
deriving instance (ToExpr ListType)
deriving instance (ToExpr OrderedStyle)
deriving instance (ToExpr ListItem)
deriving instance (ToExpr Bullet)
deriving instance (ToExpr Checkbox)
deriving instance (ToExpr SrcLine)
deriving instance (ToExpr TableRow)
deriving instance (ToExpr ColumnAlignment)
deriving instance (ToExpr OrgSection)
deriving instance (ToExpr TodoKeyword)
deriving instance (ToExpr TodoState)
deriving instance (ToExpr Priority)
deriving instance (ToExpr PlanningInfo)
