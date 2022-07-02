{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Org.Exporters.Ondim where
import Org.Types
import Org.Exporters.Common
import Ondim
import Data.Tree
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

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show, Exception)

type OrgOndimS = OndimS Exporter HTMLNode

loadOrgTemplates :: IO OrgOndimS
loadOrgTemplates = do
  files <- getFilesRecursive . (</> "templates") =<< getDataDir
  templates <- forM files $ \file -> do
    let name = takeBaseName file
    text <- readFileBS file
    case X.parseHTML file text of
      Left s -> throwIO (TemplateLoadingException s)
      Right t -> pure (fromString name, fromDocument t)
  pure $ OndimS (fromList templates <> defaultExpansions) 0 [] id

renderExpanded ::
  OrgOndimS ->
  ExporterSettings ->
  Expanded -> Either OndimException LByteString
renderExpanded st exst spl =
  runOndimT spl st
  & flip evalState defaultExporterState { exporterSettings = exst }
  <&> mapMaybe fromTree
  <&> X.renderHtmlFragment X.UTF8
  <&> toLazyByteString

renderExpansible ::
  CanExpand a =>
  OrgOndimS ->
  ExporterSettings ->
  a -> Either OndimException LByteString
renderExpansible st exst obj = renderExpanded st exst (expand obj)

renderDocWithLayout :: OrgOndimS -> X.Document -> OrgDocument -> Either OndimException LByteString
renderDocWithLayout st layout doc =
  expandDocument layout `bindingExpansions'` (documentExpansions doc)
  & flip runOndimT st
  & flip evalState st'
  <&> X.render
  <&> toLazyByteString
  where
    st' = defaultExporterState -- todo: get options from the document


data ExporterState = ExporterState
  { footnoteCounter :: (Int, Map Text Int)
  , exporterSettings :: ExporterSettings
  }

defaultExporterState :: ExporterState
defaultExporterState = ExporterState
  { footnoteCounter = (1, mempty)
  , exporterSettings = defaultExporterSettings
  }

type MonadExporter n = MonadState ExporterState n

getSetting :: MonadExporter m => (ExporterSettings -> b) -> m b
getSetting f = f <$> gets exporterSettings

getFootnoteRef :: MonadExporter n => Text -> n Text
getFootnoteRef label = do
  (i, m) <- gets footnoteCounter
  modify \s -> s { footnoteCounter = (i + 1, insert label i m) }
  pure (show i)

type Exporter = State ExporterState

type Expanded = OndimHtmlT Exporter (Forest HTMLNode)

type HTMLExpansion = Expansion Exporter HTMLNode

type HTMLExpansions = Expansions' Exporter HTMLNode

element :: Text -> Forest HTMLNode -> Tree HTMLNode
element name c = Node (Element name) c

class CanExpand a where
  expand :: a -> Expanded

instance {-# OVERLAPPABLE #-} CanExpand a => CanExpand [a] where
  expand = fmap join . mapM expand

instance (CanExpand a, CanExpand b) => CanExpand (Either a b) where
  expand = either expand expand

instance CanExpand Text where
  expand t = pure [leaf (Content t)]

instance CanExpand OrgInline where
  expand = expandOrgObject

instance CanExpand OrgElement where
  expand = expandOrgElement

instance CanExpand OrgContent where
  expand content =
    liftA2 (<>) (expand (fst content)) (sectChildren $ snd content)

instance {-# OVERLAPPABLE #-} (CanExpand a, CanExpand b) => CanExpand (a, b) where
  expand content =
    liftA2 (<>) (expand (fst content)) (expand (snd content))

instance CanExpand OrgSection where
  expand section = do
    hlevels <- getSetting orgExportHeadlineLevels
    hshift <- getSetting headlineLevelShift
    callExpansion "org:section"
      `bindingExpansions'` do
        "headline" ## headline hlevels hshift . runChildrenWith' do
          maybe (switchCases "no-todo") todo (sectionTodo section)
          "priority" ## flip (maybe silence) (sectionPriority section) \p ->
            runChildrenWith' $
              "value" ## const $ expand (priority p)
          "headline-title" ## const $ expand (sectionTitle section)
          "tags" ## \inner -> join <$> forM (sectionTags section) (`tags` inner)
        "anchor" ## const $ expand (sectionAnchor section)
        contents (sectionContent section)
    where
      todo (TodoKeyword st nm) = do
        switchCases (todost st)
        "todo-name" ## const $ expand nm
      todost Done = "done"
      todost Todo = "todo"
      priority (LetterPriority c) = T.singleton c
      priority (NumericPriority n) = show n
      headline :: Int -> Int -> Expanded -> Expanded
      headline hlevels hshift inner =
        if sectionLevel section > hlevels
        then (<> [element "br" []]) <$> inner
        else one . element ("h" <> show (hshift + sectionLevel section))
             <$> inner

documentExpansions :: OrgDocument -> HTMLExpansions
documentExpansions doc = do
  kwExps (keywordsFromList $ documentKeywords doc) "meta:"
  contents (documentContent doc)
  "footnotes" ## const do
    -- is this run in the right order? in the end?
    fnRefs <- snd <$> gets footnoteCounter
    let fns = mapMaybe (\(x,y) -> (,y) <$>
                         lookup x (documentFootnotes doc)) (toPairs fnRefs)
    if not (null fns)
    then do
      callExpansion "org:footnotes"
        `bindingExpansions'` do
          "footnote-defs" ## \inner ->
            join <$> forM fns \(fn, i) ->
              (children <$> inner)
                `bindingExpansions'` do
                  "number" ## const $ expand (show i :: Text)
                  contents fn
    else pure []

tags :: Tag -> HTMLExpansion
tags tag = runChildrenWith' $
  "tag" ## const $ expand tag

contents :: (CanExpand a) => a -> HTMLExpansions
contents c = do
  "contents" ## const $ expand c

target :: LinkTarget -> HTMLExpansions
target tgt = do
  "target" ## const $ expand $ case tgt of
     URILink "file" (changeExtension -> file)
       | isRelative file -> toText file
       | otherwise -> "file:///" <> T.dropWhile (== '/') (toText file)
     URILink protocol uri -> protocol <> ":" <> uri
     InternalLink anchor -> "#" <> anchor
     UnresolvedLink tgt' -> tgt'
  where
    changeExtension (toString -> file) =
      if takeExtension file == ".org"
      then file -<.> ".html"
      else file

kwExps :: Affiliated -> Text -> HTMLExpansions
kwExps kws prefix =
  flip foldMap (toPairs kws) \(name, kw) ->
    fromText (prefix <> name) ##
      case kw of
        ValueKeyword _ txt -> const $ expand txt
        ParsedKeyword _ c -> const $ expand c
        _ -> silence

affAttrExp :: Affiliated -> AttrExpansions Exporter
affAttrExp aff =
  "affiliated" ## const . pure $ affAttrs aff

affAttrs :: Affiliated -> [Attribute]
affAttrs aff = join $ mapMaybe getHtmlAttrs (toPairs aff)
  where
    getHtmlAttrs ("attr_html", BackendKeyword x) = Just (map Attr x)
    getHtmlAttrs _ = Nothing

expandOrgObject :: OrgInline -> Expanded
expandOrgObject = \case
  (Plain txt) ->
    expand (doSpecialStrings txt) -- TODO
  SoftBreak ->
    expand (" " :: Text)
  LineBreak ->
    pure . one $ element "br" []
  (NBSpace n) ->
    pure [leaf (RawContent $ T.replicate n "&nbsp;")]
  (Italic objs) ->
    one . element "i" <$> expand objs
  (Underline objs) ->
    one . element "u" <$> expand objs
  (Bold objs) ->
    one . element "b" <$> expand objs
  (Strikethrough objs) ->
    one . element "s" <$> expand objs
  (Superscript objs) ->
    one . element "sup" <$> expand objs
  (Subscript objs) ->
    one . element "sub" <$> expand objs
  (Quoted _ objs) ->
    one . element "q" <$> expand objs
  (Code txt) ->
    callExpansion "org:code"
    `bindingExpansions'` contents txt
  (Verbatim txt) ->
    callExpansion "org:verbatim"
    `bindingExpansions'` contents txt
  (Timestamp tsdata) ->
    timestamp tsdata
  (Entity name) ->
    expand (maybe "⍰" utf8Replacement (lookup name defaultEntitiesMap))
  (LaTeXFragment k c) ->
    latexFragment k c
  (ExportSnippet "html" c) ->
    pure [leaf (RawContent c)]
  ExportSnippet {} ->
    pure []
  (FootnoteRef label) ->
    callExpansion "org:footnote-ref"
    `bindingExpansions'` footnoteRef label
  (Cite _) -> expand ("(unresolved citation)" :: Text)
  InlBabelCall {} ->
    pure []
  (Src lang _ txt) ->
    callExpansion "org:src"
    `bindingExpansions'` do
      "language" ## const $ expand lang
      contents txt
  (Link tgt inl) ->
    callExpansion "org:link"
    `bindingExpansions'` (target tgt <> contents inl)
  (Image tgt) ->
    callExpansion "org:image" `bindingExpansions'` (target tgt)
  (Target uid) ->
    pure [toTree $ X.Element "a" [("id", uid)] []]
  Macro {} ->
    pure []

expandOrgElement :: OrgElement -> Expanded
expandOrgElement = \case
  (Paragraph aff [Image tgt]) ->
    callExpansion "org:figure"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` (kwExps aff "kw:" <> target tgt)
  (Paragraph aff c) ->
    callExpansion "org:paragraph"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` contents c
  (GreaterBlock aff Quote c) ->
    callExpansion "org:quote-block"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` contents c
  (GreaterBlock aff Center c) ->
    callExpansion "org:center-block"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` contents c
  (GreaterBlock aff (Special cls) c) ->
    callExpansion "org:special-block"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` do
      "special-name" ## const $ expand cls
      contents c
  (PlainList aff k i) -> plainList k i
    `bindingExpansions` affAttrExp aff
  (DynamicBlock _ _ els) -> expand els
  (Drawer _ els) -> expand els
  (ExportBlock "html" c) -> pure [leaf (RawContent c)]
  (ExportBlock _ _) -> pure []
  (ExampleBlock aff st c) ->
    callExpansion "org:example-block"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` srcOrExample st c
  (SrcBlock aff lang st _ c) ->
    callExpansion "org:src-block"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` do
       "language" ## const $ expand lang
       srcOrExample st c
  (LaTeXEnvironment aff _ text) ->
    callExpansion "org:latex-environment"
    `bindingExpansions` affAttrExp aff
    `bindingExpansions'` contents text
  HorizontalRule -> pure . one $ element "hr" []
  Keyword {} -> pure []
  VerseBlock {} -> error "not impl"
  Clock {} -> error "not impl"
  -- Table {} -> error "Table html export is not yet implemented :( please help"

sectChildren :: [OrgSection] -> Expanded
sectChildren childs = do
  hlevels <- getSetting orgExportHeadlineLevels
  flip (maybe (pure [])) (viaNonEmpty (sectionLevel . head) childs) \ level ->
    if level > hlevels
    then one . element "ol" <$> mapM (fmap (element "li") . expand) childs
    else expand childs


plainList :: ListType -> [ListItem] -> Expanded
plainList kind items = do
  switch plainListTag $ Node emptyValue <$> callExpansion "org:plain-list"
    `bindingExpansions'` do
      "list-items" ## listItems
      "bullet" ## const bullet
      "counter" ## const counter
  where
    listItems :: HTMLExpansion
    listItems inner =
      join <$> forM items \(ListItem _ i cbox t c) ->
        (children <$> inner)
        `bindingExpansions'` do
          "counter-set" ## maybe silence counterSet i
          "checkbox" ## maybe silence checkbox cbox
          "tag" ## const $ expand t
          "contents" ## const $ expand $ cropRenderElements c
      where
        counterSet i _ = expand (show i :: Text)

        cropRenderElements (Paragraph _ objs : rest) = Left (objs, rest)
        cropRenderElements els = Right els

        checkbox (BoolBox True) _ = callExpansion "checkbox:true"
        checkbox (BoolBox False) _ = callExpansion "checkbox:false"
        checkbox PartialBox  _ = callExpansion "checkbox:partial"

    plainListTag = case kind of
      Ordered _   -> "ordered"
      Descriptive -> "descriptive"
      Unordered _ -> "unordered"

    bullet = case kind of
      Unordered b -> callExpansion (fromText $ "bullet:" `T.snoc` b)
      _ -> pure []

    counter = case kind of
      Ordered OrderedNum -> callExpansion "counter:num"
      Ordered OrderedAlpha -> callExpansion "counter:alpha"
      _ -> pure []

srcOrExample :: Maybe Int -> [SrcLine] -> HTMLExpansions
srcOrExample stNumber lins = do
  "src-lines" ## runLines
  where
    runLines :: HTMLExpansion
    runLines inner =
      intersperse (leaf (Content "\n")) <$>
      foldMapM (flip lineSplices inner) (zip [0..] lins)

    lineNumber offset = do
      "line-number" ##
        flip (maybe silence) ((offset +) <$> stNumber) \n ->
          runChildrenWith' ("number" ## const $ expand (show n :: Text))

    lineSplices (offset, SrcLine c) inner = switch "plain" inner
      `bindingExpansions'` do
        lineNumber offset
        contents c

    lineSplices (offset, RefLine i ref c) inner = switch "ref" inner
      `bindingExpansions'` do
        lineNumber offset
        "ref" ## const $ expand ref
        "id"  ## const $ expand i
        contents c

footnoteRef :: Text -> HTMLExpansions
footnoteRef label = do
  "counter" ## const (expand =<< getFootnoteRef label)

timestamp :: TimestampData -> Expanded
timestamp = \case
  TimestampData a (dateToDay -> d, fmap toTime -> t, r, w) ->
    switch (active a <> "-single") $ Node emptyValue <$>
      callExpansion "org:timestamp"
      `bindingExpansions'` dtExps d t r w
  TimestampRange a (dateToDay -> d1, fmap toTime -> t1, r1, w1)
                   (dateToDay -> d2, fmap toTime -> t2, r2, w2) ->
    switch (active a <> "-range") $ Node emptyValue <$>
      callExpansion "org:timestamp"
      `bindingExpansions'` do
        "from" ## runChildrenWith' (dtExps d1 t1 r1 w1)
        "to"   ## runChildrenWith' (dtExps d2 t2 r2 w2)
  where
    dtExps d t r w = do
      "repeater" ## maybe silence (runChildrenWith' . tsMark) r
      "warning-period" ## maybe silence (runChildrenWith' . tsMark) w
      mapV fromReprExpansion $ do
        tsDate d
        tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> HTMLExpansions
    tsMark (_,v,c) = do
      "value" ## const $ expand (show v :: Text)
      "unit" ## const $ callExpansion (fromText $ "unit:" `T.snoc` c)

    dateToDay (y,m,d,_) = fromGregorian (toInteger y) m d
    toTime (h,m) = TimeOfDay h m 0

    tsDate :: Day -> NodeExpansions Exporter
    tsDate day = "ts-date" ## \input' -> do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ X.nodeText input
      pure . one . X.TextNode . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> NodeExpansions Exporter
    tsTime time = "ts-time" ## \input' -> do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ X.nodeText input
      maybe (pure []) (pure . one . X.TextNode . toText . formatTime locale format) time

latexFragment :: FragmentType -> Text -> Expanded
latexFragment kind txt =
  switch (case kind of
            InlMathFragment -> "inline"
            DispMathFragment -> "display"
            RawFragment -> "raw") $
  Node emptyValue <$>
  callExpansion "org:latex-fragment"
  `bindingExpansions'` contents txt
