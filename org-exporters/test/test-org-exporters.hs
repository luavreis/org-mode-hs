{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as KM
import Ondim (OndimNode, OndimState, binding, callTemplate, evalOndimTWith)
import Ondim.Extra.Exceptions (prettyException)
import Ondim.Targets.HTML qualified as H
import Ondim.Targets.LaTeX qualified as L
import Ondim.Targets.Pandoc qualified as P
import Org.Exporters.Common (ExportBackend, documentExp, renderNode)
import Org.Exporters.Data.Templates (templatesEmbed)
import Org.Exporters.HTML qualified as H
import Org.Exporters.LaTeX qualified as L
import Org.Exporters.Pandoc qualified as P
import Org.Exporters.Processing (OrgData, processAll)
import Org.Parser (defaultOrgOptions, parseOrgDoc)
import Org.Types.Variants.Annotated (OrgDocument)
import Relude.Unsafe (fromJust)
import System.Directory qualified as D
import System.FilePath (takeBaseName, (</>))
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Bench
import Test.Tasty.Golden (goldenVsStringDiff)
import Text.Pandoc.Definition (Pandoc)

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
  IO (OrgDocument, OrgData) ->
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
      toRight $
        evalOndimTWith tpls $
          callTemplate @a ("document." <> ext)
            `binding` documentExp bk datum doc
    f = fromJust renderNode
    toRight = (either (error . prettyException) return =<<)

testFile :: FilePath -> Benchmark
testFile dir = do
  withResource loadOrg (const pass) \textIO ->
    testGroup
      (takeBaseName dir)
      [ bench "Parse" $ nfIO $ parseOrg <$> textIO
      , withResource (parseOrg <$> textIO) (const pass) \parsed ->
          testGroup
            "Process"
            [ bench "process" $ nfIO $ processAll <$> parsed
            , withResource (processOrg <$> parsed) (const pass) \stuff ->
                testGroup
                  "Export"
                  [ benchsFor @H.HtmlDocument tplsHtml "html" (out "html") H.defBackend stuff f
                  , benchsFor @Pandoc tplsPandoc "md" (out "json") P.defBackend stuff json
                  , benchsFor @[L.Node] tplsLaTeX "tex" (out "tex") L.defBackend stuff f
                  ]
            ]
      ]
  where
    file = dir </> "in.org"
    out ext = dir </> "out." ++ ext
    loadOrg :: IO Text = decodeUtf8 <$> readFileBS file
    parseOrg = parseOrgDoc defaultOrgOptions file
    processOrg = processAll
    f :: OndimNode a => a -> LByteString
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
