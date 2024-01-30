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

import Data.Ix.RecursionSchemes (Fix (..))
import Data.TreeDiff
import Data.TreeDiff.Golden (ediffGolden)
import Data.TreeDiff.OMap qualified as OM
import Org.Parser
import Org.Parser.Objects (Marked (..))
import Org.Types.Variants.ParseInfo
import Org.Types.Variants.Plain qualified as P
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Test.Tasty.Options
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
  parse' p = parse' p.parser

prettyParse :: (Parsable m, ToExpr a) => m a -> Text -> IO ()
prettyParse parser txt =
  case parse' parser txt of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> print $ ansiWlBgExpr $ toExpr x

infix 1 =?>

(=?>) :: a -> b -> (a, Either () b)
x =?> y = (x, Right y)

infix 1 =!>

(=!>) :: a -> () -> (a, Either () c)
x =!> y = (x, Left y)

(~:) ::
  (HasCallStack) =>
  (Parsable m, Eq a, ToExpr a) =>
  TestName ->
  m a ->
  [Text] ->
  TestTree
(~:) name parser cases =
  testGroup name
    $ flip (`zipWith` [1 ..]) cases
    $ \(i :: Int) txt ->
      askOption \(GoldenPath outDir) ->
        ediffGolden goldenTest (name <> " " <> show i) (outDir </> name <.> show i)
          $ return
          $ first errorBundlePretty
          $ parse' parser txt

newtype GoldenPath = GoldenPath String

instance IsOption GoldenPath where
  defaultValue = GoldenPath "test/files/golden"
  parseValue = Just . GoldenPath
  optionName = "golden-path"
  optionHelp = "Path to golden test files."

goldenGroup :: TestName -> [TestTree] -> TestTree
goldenGroup tn = adjustOption f . testGroup tn
  where
    f (GoldenPath p) = GoldenPath (p </> tn)

deriving instance (ToExpr OrgDocument)
deriving instance (ToExpr (KeywordValue OrgObjects))
deriving instance (ToExpr QuoteType)
deriving instance (ToExpr TimestampData)
deriving instance (ToExpr OrgDate)
deriving instance (ToExpr OrgTime)
deriving instance (ToExpr FragmentType)
deriving instance (ToExpr (FootnoteRefData OrgObjects))
deriving instance (ToExpr (Citation OrgObjects))
deriving instance (ToExpr (CiteReference OrgObjects))
deriving instance (ToExpr BabelCall)
deriving instance (ToExpr LinkTarget)
deriving instance (ToExpr (OrgElementData Org ix))
deriving instance (ToExpr (OrgObjectData Org ix))
deriving instance (ToExpr (OrgSectionData Org ix))
deriving instance (ToExpr GreaterBlockType)
deriving instance (ToExpr ListType)
deriving instance (ToExpr OrderedStyle)
deriving instance (ToExpr (ListItem Org ix))
deriving instance (ToExpr Bullet)
deriving instance (ToExpr Checkbox)
deriving instance (ToExpr (TableRow OrgObjects))
deriving instance (ToExpr ColumnAlignment)
deriving instance (ToExpr (OrgF Org ix))
deriving instance (ToExpr StandardProperties)
deriving instance (ToExpr TodoKeyword)
deriving instance (ToExpr TodoState)
deriving instance (ToExpr Priority)
deriving instance (ToExpr PlanningInfo)

instance ToExpr (P.OrgF Org ix) where
  toExpr = \case
    P.OrgObjectF d -> toExpr d
    P.OrgElementF a d -> Rec "OrgElement" (OM.fromList [("affiliated", toExpr a), ("data", toExpr d)])
    P.OrgSectionF d -> toExpr d

instance ToExpr (Org ix) where
  toExpr (Fix (ComposeIx x)) = Lst $ map toExpr $ toList x
