{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as KM
import Data.Ix.RecursionSchemes (Fix (..))
import Data.TreeDiff.Class (ToExpr (..))
import Data.TreeDiff.Expr (Expr (..))
import Data.TreeDiff.Golden (ediffGolden)
import Data.TreeDiff.OMap qualified as OM
import Ondim (OndimNode, OndimState, binding, callTemplate, evalOndimWith)
import Ondim.Extra.Exceptions (prettyException)
import Ondim.Targets.HTML qualified as H
import Ondim.Targets.LaTeX qualified as L
import Ondim.Targets.Pandoc qualified as P
import Org.Exporters.Common (ExportBackend, documentExp, renderNode)
import Org.Exporters.Data.Templates (templatesEmbed)
import Org.Exporters.HTML qualified as H
import Org.Exporters.LaTeX qualified as L
import Org.Exporters.Pandoc qualified as P
import Org.Exporters.Processing (OrgData, processAll, ExporterSettings)
import Org.Parser (defaultOrgOptions, parseOrgDoc, OrgOptions)
import Org.Types.Data.Element
import Org.Types.Data.Object
import Org.Types.Data.Section
import Org.Types.Data.StandardProperties
import Org.Types.Data.Timestamp
import Org.Types.Ix (AllOrgIx, ComposeIx (..))
import Org.Types.Variants.Annotated (OrgDocumentData)
import Org.Types.Variants.Annotated qualified as A
import Org.Types.Variants.ParseInfo qualified as PI
import Org.Types.Variants.Plain qualified as P
import Relude.Unsafe (fromJust)
import System.Directory qualified as D
import System.FilePath (takeBaseName, (<.>), (</>))
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Bench
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Providers (TestName, TestTree)
import Text.Pandoc.Definition (Pandoc)
import Citeproc.CslJson (CslJson)

tplsPandoc :: OndimState IO
tplsPandoc = templatesEmbed [P.loadPandocMd]

tplsHtml :: OndimState IO
tplsHtml = templatesEmbed [H.loadHtml]

tplsLaTeX :: OndimState IO
tplsLaTeX = templatesEmbed [L.loadLaTeX]

benchsFor ::
  forall a.
  (OndimNode a, NFData a) =>
  OndimState IO ->
  Text ->
  String ->
  ExportBackend IO ->
  IO (A.OrgDocument, OrgData) ->
  (a -> LByteString) ->
  Benchmark
benchsFor tpls ext out bk bundle pf =
  testGroup
    (toString ext)
    [ bench "export" $ nfIO expanded
    , withResource expanded (const pass) \doc ->
        testGroup
          "render"
          [ bench "render" $ nfIO $ f <$> doc
          , diffTest $ foldMap' pf <$> doc
          ]
    ]
  where
    diffTest = goldenVsStringDiff "golden" (\ref new -> ["git", "diff", "--color", "--word-diff=plain", ref, new]) out
    expanded = do
      (doc, datum) <- bundle
      toRight
        $ evalOndimWith tpls
        $ callTemplate @a ("document." <> ext)
        `binding` documentExp bk datum doc
    f = fromJust renderNode
    toRight = (either (error . prettyException) return =<<)

testFile :: FilePath -> Benchmark
testFile dir = do
  withResource loadOrg (const pass) \textIO ->
    testGroup
      (takeBaseName dir)
      [ withResource (parseOrg <$> textIO) (const pass) \parsed ->
          testGroup
            "parsing"
            [ bench "bench" $ nfIO $ parseOrg <$> textIO
            , diffGolden "parsed" parsed
            , withResource (processOrg <$> parsed) (const pass) \stuff ->
                testGroup
                  "processing"
                  [ bench "bench" $ nfIO $ processAll <$> parsed
                  , diffGolden "processed" stuff
                  , testGroup
                      "export"
                      [ benchsFor @H.HtmlDocument tplsHtml "html" (out "html") H.defBackend stuff f
                      , benchsFor @Pandoc tplsPandoc "md" (out "json") P.defBackend stuff json
                      , benchsFor @[L.Node] tplsLaTeX "tex" (out "tex") L.defBackend stuff f
                      ]
                  ]
            ]
      ]
  where
    diffGolden :: (Eq a, ToExpr a) => TestName -> IO a -> TestTree
    diffGolden name = ediffGolden goldenTest "golden" (dir </> name)
    file = dir </> "in.org"
    out ext = dir </> "out" <.> ext
    loadOrg :: IO Text = decodeUtf8 <$> readFileBS file
    parseOrg = parseOrgDoc defaultOrgOptions file
    processOrg = processAll
    f :: (OndimNode a) => a -> LByteString
    f = fromJust renderNode
    json = encodePretty . filt . toJSON
      where
        -- The API version does not tell us much.
        filt (Object o) = Object $ KM.delete "pandoc-api-version" o
        filt x = x

main :: IO ()
main = do
  tests <- D.listDirectory "test/files"
  defaultMain (testFile . ("test/files" </>) <$> tests)

deriving instance (ToExpr o) => (ToExpr (KeywordValue o))
deriving instance (ToExpr QuoteType)
deriving instance (ToExpr TimestampData)
deriving instance (ToExpr OrgDate)
deriving instance ToExpr OrgTime
deriving instance (ToExpr FragmentType)
deriving instance (ToExpr o) => ToExpr (FootnoteRefData o)
deriving instance (ToExpr o) => ToExpr (Citation o)
deriving instance (ToExpr o) => ToExpr (CiteReference o)
deriving instance (ToExpr BabelCall)
deriving instance (ToExpr LinkTarget)
deriving instance (AllOrgIx ToExpr k) => ToExpr (OrgElementData k ix)
deriving instance (AllOrgIx ToExpr k) => ToExpr (OrgObjectData k ix)
deriving instance (AllOrgIx ToExpr k) => ToExpr (OrgSectionData k ix)
deriving instance (AllOrgIx ToExpr k) => ToExpr (OrgDocumentData k ix)
deriving instance (ToExpr GreaterBlockType)
deriving instance (ToExpr ListType)
deriving instance (ToExpr OrderedStyle)
deriving instance (AllOrgIx ToExpr k) => ToExpr (ListItem k ix)
deriving instance (ToExpr Bullet)
deriving instance (ToExpr Checkbox)
deriving instance (ToExpr o) => (ToExpr (TableRow o))
deriving instance (ToExpr ColumnAlignment)
deriving instance (AllOrgIx ToExpr k) => (ToExpr (PI.OrgF k ix))
deriving instance (AllOrgIx ToExpr k) => (ToExpr (A.OrgF k ix))
deriving instance (ToExpr StandardProperties)
deriving instance (ToExpr TodoKeyword)
deriving instance (ToExpr TodoState)
deriving instance (ToExpr Priority)
deriving instance (ToExpr PlanningInfo)
deriving instance (ToExpr OrgData)
deriving instance (ToExpr (CslJson Text))
deriving instance (ToExpr ExporterSettings)
deriving instance (ToExpr OrgOptions)
deriving instance (ToExpr (InternalLink Text))

instance (AllOrgIx ToExpr k) => ToExpr (P.OrgF k ix) where
  toExpr = \case
    P.OrgObjectF d -> toExpr d
    P.OrgElementF a d -> Rec "OrgElement" (OM.fromList [("affiliated", toExpr a), ("data", toExpr d)])
    P.OrgSectionF d -> toExpr d

instance ToExpr (P.Org ix) where
  toExpr (Fix (ComposeIx x)) = Lst $ map toExpr $ toList x

instance ToExpr (PI.Org ix) where
  toExpr (Fix (ComposeIx x)) = Lst $ map toExpr $ toList x

instance ToExpr (A.Org ix) where
  toExpr (Fix (ComposeIx x)) = Lst $ map toExpr x
