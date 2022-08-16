{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Org.Exporters.HTML where
import Org.Types
import Org.Exporters.Common
import Ondim
import Ondim.HTML
import Ondim.Extra
import Data.Map.Syntax
import Data.Text qualified as T
import Text.XmlHtml qualified as X
import Relude.Extra.Map
import System.FilePath
import Org.Data.Entities (defaultEntitiesMap, utf8Replacement)
import Data.Time (Day, TimeOfDay (..), fromGregorian, formatTime)
import Paths_org_parser
import System.Directory.Recursive
import Control.Exception (throwIO)
import Data.ByteString.Builder (toLazyByteString)

type HTag = HtmlTag (State ExporterState)

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show, Exception)

type OrgOndimS = OndimS HTag HtmlNode

type HtmlExpansion = Expansion HTag HtmlNode

type HtmlExpansions = Expansions' HTag HtmlNode

emptyNode :: Ondim HTag HtmlNode
emptyNode = pure $ TextNode ""

loadHtmlTemplates :: IO (OndimS HTag HtmlNode)
loadHtmlTemplates = do
  files <- getFilesRecursive . (</> "templates/html") =<< getDataDir
  templates <- forM files $ \file -> do
    let name = takeBaseName file
    text <- readFileBS file
    case X.parseHTML file text of
      Left s -> throwIO (TemplateLoadingException s)
      Right t -> pure (fromString name, fromDocument t)
  pure $ OndimS { expansions = fromList templates
                , filters = mempty
                }

renderExpanded ::
  ExporterSettings ->
  OrgOndimS ->
  Ondim HTag [HtmlNode] -> Either OndimException LByteString
renderExpanded exst st spl = spl
  & withOndimS (const st)
  & bindDefaults
  & runOndimT
  & flip evalState defaultExporterState { exporterSettings = exst }
  & second toNodeList
  <&> X.renderHtmlFragment X.UTF8
  <&> toLazyByteString

renderDocWithLayout :: OrgOndimS -> X.Document -> OrgDocument -> Either OndimException LByteString
renderDocWithLayout st layout doc =
  liftSubstructures layout `binding` (documentExpansions doc)
  & bindDefaults
  & withOndimS (const st)
  & runOndimT
  & flip evalState st'
  <&> X.render
  <&> toLazyByteString
  where
    st' = defaultExporterState -- todo: get options from the document

element :: Text -> [HtmlNode] -> HtmlNode
element name c = Element False name [] c

expandOrgSections :: [OrgSection] -> Ondim HTag [HtmlNode]
expandOrgSections [] = pure []
expandOrgSections sections@(OrgSection{sectionLevel = level} : _) = do
  hlevels <- getSetting orgExportHeadlineLevels
  callExpansion "org:sections" emptyNode
    `binding` do
      if level > hlevels
      then switchCases @HtmlNode "over-level"
      else switchCases @HtmlNode "normal"
      "sections" ## \x ->
        join <$> forM sections \section ->
          children x
            `binding` do
              "headline-title" ##
                const $ expandOrgObjects (sectionTitle section)
              "tags" ## \inner ->
                join <$> forM (sectionTags section) (`tags` inner)
              "h-n" ## \y -> fmap one $ Element True ("h" <> show level) <$> attributes y <*> children y
              "children" ## const $ expandOrgElements (sectionChildren section)
              "subsections" ## const $ expandOrgSections (sectionSubsections section)
            `bindingText` do
              for_ (sectionTodo section) todo
              for_ (sectionPriority section) priority
              "anchor" ## pure $ sectionAnchor section
  where
    todo (TodoKeyword st nm) = do
      "todo-state" ## pure (todost st)
      "todo-name" ## pure nm
    todost Done = "done"
    todost Todo = "todo"
    priority p = "priority" ## pure case p of
      (LetterPriority c) -> T.singleton c
      (NumericPriority n) -> show n

documentExpansions :: OrgDocument -> HtmlExpansions
documentExpansions doc = do
  parsedKwExpansions
    expandOrgObjects
    (keywordsFromList $ documentKeywords doc)
    "meta:"
  "children" ## const $ expandOrgElements (documentChildren doc)
  "sections" ## const $ expandOrgSections (documentSections doc)
  "footnotes" ## const do
    -- is this run in the right order? in the end?
    fnRefs <- snd <$> gets footnoteCounter
    let fns = mapMaybe (\(x,y) -> (,y) <$>
                         lookup x (documentFootnotes doc)) (toPairs fnRefs)
    if not (null fns)
    then
      callExpansion "org:footnotes" emptyNode
        `binding` do
          "footnote-defs" ## \inner ->
            join <$> forM fns \(fn, i) ->
              children @HtmlNode inner
              `bindingText` do
                "number" ## pure $ show i
              `binding` do
                "footnote-def" ## const $ expandOrgElements fn
    else pure []

expandOrgObjects :: [OrgObject] -> Ondim HTag [HtmlNode]
expandOrgObjects = foldMapM expandOrgObject

expandOrgObject :: OrgObject -> Ondim HTag [HtmlNode]
expandOrgObject = \case
  (Plain txt) ->
    pure . one $ TextNode (doSpecialStrings txt) -- TODO
  SoftBreak ->
    pure . one $ TextNode (" " :: Text)
  LineBreak ->
    pure $ one $ element "br" []
  (Italic objs) ->
    one . element "i" <$> expandOrgObjects objs
  (Underline objs) ->
    one . element "u" <$> expandOrgObjects objs
  (Bold objs) ->
    one . element "b" <$> expandOrgObjects objs
  (Strikethrough objs) ->
    one . element "s" <$> expandOrgObjects objs
  (Superscript objs) ->
    one . element "sup" <$> expandOrgObjects objs
  (Subscript objs) ->
    one . element "sub" <$> expandOrgObjects objs
  (Quoted _ objs) ->
    one . element "q" <$> expandOrgObjects objs
  (Code txt) ->
    callExpansion "org:code" emptyNode
    `bindingText` do "content" ## pure txt
  (Verbatim txt) ->
    callExpansion "org:verbatim" emptyNode
    `bindingText` do "content" ## pure txt
  (Timestamp tsdata) ->
    timestamp tsdata
  (Entity name) ->
    pure . one $ TextNode (maybe "⍰" utf8Replacement (lookup name defaultEntitiesMap))
  (LaTeXFragment k c) ->
    latexFragment k c
  (ExportSnippet "html" c) ->
    pure [rawNode c]
  ExportSnippet {} ->
    pure []
  (FootnoteRef label) ->
    callExpansion "org:footnote-ref" emptyNode
    `bindingText` footnoteRef label
  (Cite _) ->
    error "(unresolved citation)" -- TODO
  InlBabelCall {} ->
    pure []
  (Src lang _ txt) ->
    callExpansion "org:src" emptyNode
    `bindingText` do
      "language" ## pure lang
      "content" ## pure txt
  (Link tgt inl) ->
    callExpansion "org:link" emptyNode
    `bindingText` linkTarget tgt
    `binding` do "content" ## const $ expandOrgObjects inl
  (Image tgt) ->
    callExpansion "org:image" emptyNode
    `bindingText` linkTarget tgt
  (Target uid) ->
    pure [Element False "a" [("id", uid)] []]
  Macro {} ->
    pure []

expandOrgElements :: [OrgElement] -> Ondim HTag [HtmlNode]
expandOrgElements = foldMapM expandOrgElement

expandOrgElement :: OrgElement -> Ondim HTag [HtmlNode]
expandOrgElement = \case
  (Paragraph aff [Image tgt]) ->
    callExpansion "org:figure" emptyNode
    `bindingAff` aff
    `bindingText` linkTarget tgt
  (Paragraph aff c) ->
    callExpansion "org:paragraph" emptyNode
    `bindingAff` aff
    `binding` ("content" ## const $ toList <$> expandOrgObjects c)
  (GreaterBlock aff Quote c) ->
    callExpansion "org:quote-block" emptyNode
    `bindingAff` aff
    `binding` content c
  (GreaterBlock aff Center c) ->
    callExpansion "org:center-block" emptyNode
    `bindingAff` aff
    `binding` content c
  (GreaterBlock aff (Special cls) c) ->
    callExpansion "org:special-block" emptyNode
    `bindingAff` aff
    `bindingText` do "special-name" ## pure cls
    `binding` content c
  (PlainList aff k i) ->
    callExpansion "org:plain-list" emptyNode
    `plainList` (k, i)
    `bindingAff` aff
  (DynamicBlock _ _ els) ->
    expandOrgElements els
  (Drawer _ els) ->
    expandOrgElements els
  (ExportBlock "html" c) ->
    pure [rawNode c]
  (ExportBlock _ _) ->
    pure []
  (ExampleBlock aff st c) ->
    callExpansion "org:example-block" emptyNode
    `bindingAff` aff
    `srcOrExample` (st, c)
  (SrcBlock aff lang st _ c) ->
    callExpansion "org:src-block" emptyNode
    `bindingAff` aff
    `bindingText` do "language" ## pure lang
    `srcOrExample` (st, c)
  (LaTeXEnvironment aff _ text) ->
    callExpansion "org:latex-environment" emptyNode
    `bindingAff` aff
    `bindingText` do "content" ## pure text
  HorizontalRule -> pure . one $ element "hr" []
  Keyword {} -> pure []
  VerseBlock {} -> error "not impl"
  Clock {} -> error "not impl"
  -- Table {} -> error "Table html export is not yet implemented :( please help"
  where bindingAff x aff = x
          `binding` affiliatedAttrExpansions "html" aff
          `binding` parsedKwExpansions expandOrgObjects aff "kw:"
          `bindingText` textKwExpansions aff "kw:"
        content x = "content" ## const $ expandOrgElements x

plainList :: Ondim HTag t -> (ListType, [ListItem]) -> Ondim HTag t
plainList x (kind, items) = x
  `binding` do
    "list-items" ## listItems
    switchCases
      case kind of
        Ordered _   -> "ordered"
        Descriptive -> "descriptive"
        Unordered _ -> "unordered"
  `bindingText`
    case kind of
      Ordered style ->
        "counter" ## pure
          case style of
            OrderedNum -> "num"
            OrderedAlpha -> "alpha"
      Unordered b ->
        "bullet" ## pure (one b)
      _ -> mempty
  where
    listItems :: Expansion HTag HtmlNode
    listItems inner =
      join <$> forM items \(ListItem _ i cbox t c) ->
        children inner
        `bindingText` do
          "counter-set" ## pure $ maybe "" show i
          "checkbox" ## pure $ maybe "" checkbox cbox
        `binding` do
          "descriptive-tag" ## const $ expandOrgObjects t
          "list-item-content" ## const $ doPlainOrPara c
      where
        doPlainOrPara [Paragraph _ objs] = expandOrgObjects objs
        doPlainOrPara els = expandOrgElements els

        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox  = "partial"

srcOrExample :: Ondim HTag t -> (Maybe Int, [SrcLine]) -> Ondim HTag t
srcOrExample x (stNumber, lins) = x
  `binding` ("src-lines" ## runLines)
  `bindingText` ("content" ## pure $ T.intercalate "\n" (srcLineContent <$> lins))
  where
    runLines :: HtmlExpansion
    runLines inner =
      intersperse (TextNode "\n") <$>
      foldMapM (flip lineExps inner) (zip [0..] lins)

    number offset =
      whenJust ((offset +) <$> stNumber) \n ->
        "number" ## pure $ show n

    lineExps (offset, SrcLine c) inner =
      switch "plain" inner
      `bindingText` do
        number offset
        "content" ## pure c

    lineExps (offset, RefLine i ref c) inner =
      switch "ref" inner
      `bindingText` do
        number offset
        "ref" ## pure ref
        "id"  ## pure i
        "content" ## pure c

footnoteRef :: MonadState ExporterState (OndimMonad t) => Text -> MapSyntax Text (Ondim t Text)
footnoteRef label = do
  "counter" ## getFootnoteRef label

timestamp :: TimestampData -> Ondim HTag [HtmlNode]
timestamp = \case
  TimestampData a (dateToDay -> d, fmap toTime -> t, r, w) ->
    callExpansion "org:timestamp" emptyNode
    `binding` do
      dtExps d t r w
      switchCases (active a <> "-single")
  TimestampRange a (dateToDay -> d1, fmap toTime -> t1, r1, w1)
                   (dateToDay -> d2, fmap toTime -> t2, r2, w2) ->
    callExpansion "org:timestamp" emptyNode
    `binding` do
      "from" ## \x -> children x `binding` dtExps d1 t1 r1 w1
      "to"   ## \x -> children x `binding` dtExps d2 t2 r2 w2
      switchCases @HtmlNode (active a <> "-range")
  where
    dtExps d t r w = do
      "repeater" ##
        justOrIgnore r \r' x -> children x `bindingText` tsMark r'
      "warning-period" ##
        justOrIgnore w \w' x -> children x `bindingText` tsMark w'
      tsDate d
      tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> MapSyntax Text (Ondim HTag Text)
    tsMark (_,v,c) = do
      "value" ## pure $ show v
      "unit" ## pure $ one c

    dateToDay (y,m,d,_) = fromGregorian (toInteger y) m d
    toTime (h,m) = TimeOfDay h m 0

    tsDate :: Day -> HtmlExpansions
    tsDate day = "ts-date" ## \input' -> do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ nodeText input
      pure . one . TextNode . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> HtmlExpansions
    tsTime time = "ts-time" ## \input' -> do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ nodeText input
      maybe (pure []) (pure . one . TextNode . toText . formatTime locale format) time

latexFragment :: FragmentType -> Text -> Ondim HTag [HtmlNode]
latexFragment kind txt =
  callExpansion "org:latex-fragment" emptyNode
  `bindingText` do
    "content" ## pure txt
  `binding` do
    switchCases @HtmlNode $
      case kind of
        InlMathFragment -> "inline"
        DispMathFragment -> "display"
        RawFragment -> "raw"
