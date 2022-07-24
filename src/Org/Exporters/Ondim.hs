{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Org.Exporters.Ondim where
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

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show, Exception)

type Exporter = State ExporterState

type HTag = HtmlTag Exporter

emptyNode :: X.Node
emptyNode = X.TextNode ""

justOrIgnore :: OndimTag tag => Maybe a -> (a -> Expansion tag b) -> Expansion tag b
justOrIgnore = flip (maybe ignore)

textExpansion :: Text -> HTMLExpansion
textExpansion = const . pure . one . X.TextNode

bindingHtmlText :: Ondim HTag a -> MapSyntax Text Text -> Ondim HTag a
bindingHtmlText n t = n
  `bindingText` t
  `binding` mapV textExpansion t

loadOrgTemplates :: IO (OndimS HTag X.Node -> OndimS HTag X.Node)
loadOrgTemplates = do
  files <- getFilesRecursive . (</> "templates") =<< getDataDir
  templates <- forM files $ \file -> do
    let name = takeBaseName file
    text <- readFileBS file
    case X.parseHTML file text of
      Left s -> throwIO (TemplateLoadingException s)
      Right t -> pure (fromString name, fromDocument t)
  pure $ \os -> os { expansions = fromList templates <> expansions os
                   , afterExpansion = trimSpaces
                   }

trimSpaces :: [X.Node] -> [X.Node]
trimSpaces tpl = removeEmpty tpl &
                 concatContents &
                 changeLast (mapText T.stripEnd) &
                 changeFst (mapText T.stripStart)
  where
    removeEmpty = filter notEmpty
      where
        notEmpty (X.TextNode "") = False
        notEmpty X.Comment {} = False
        notEmpty _ = True

    concatContents (X.TextNode t1 : X.TextNode t2 : xs) =
      concatContents (X.TextNode (t1 <> t2) : xs)
    concatContents (x : xs) = x : concatContents xs
    concatContents [] = []

    changeLast _ [] = []
    changeLast f [x] = [f x]
    changeLast f (x:xs) = x : changeLast f xs

    changeFst _ [] = []
    changeFst f (x:xs) = f x : xs

    mapText f (X.TextNode t) = X.TextNode (f t)
    mapText _ x = x

renderExpanded ::
  ExporterSettings ->
  Expanded -> Either OndimException LByteString
renderExpanded exst spl =
  runOndimT spl
  & flip evalState defaultExporterState { exporterSettings = exst }
  <&> X.renderHtmlFragment X.UTF8
  <&> toLazyByteString

renderExpansible ::
  CanExpand a =>
  ExporterSettings ->
  a -> Either OndimException LByteString
renderExpansible exst obj = renderExpanded exst (expand obj)

renderDocWithLayout :: X.Document -> OrgDocument -> Either OndimException LByteString
renderDocWithLayout layout doc =
  expandDocument layout `binding` (documentExpansions doc)
  & runOndimT
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

type Expanded = Ondim HTag [X.Node]

type HTMLExpansion = Expansion HTag X.Node

type HTMLExpansions = Expansions' HTag X.Node

element :: Text -> [X.Node] -> X.Node
element name c = X.Element name [] c

class CanExpand a where
  expand :: a -> Expanded

instance {-# OVERLAPPABLE #-} CanExpand a => CanExpand [a] where
  expand = fmap join . mapM expand

instance (CanExpand a, CanExpand b) => CanExpand (Either a b) where
  expand = either expand expand

instance CanExpand Text where
  expand t = pure [X.TextNode t]

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
    callExpansion "org:section" emptyNode
      `binding` do
        "headline" ## headline hlevels hshift . \x ->
          X.childNodes <$> x `binding` do
            maybe (switchCases "no-todo") todo (sectionTodo section)
            "priority" ## justOrIgnore (sectionPriority section) \p y ->
              X.childNodes <$> y `binding` do
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
      callExpansion "org:footnotes" emptyNode
        `binding` do
          "footnote-defs" ## \inner ->
            join <$> forM fns \(fn, i) ->
              X.childNodes <$> inner
              `bindingHtmlText` do
                "number" ## show i
              `binding`
                contents fn
    else pure []

tags :: Tag -> HTMLExpansion
tags tag x = X.childNodes <$> x `binding` do
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
    (prefix <> name) ##
      case kw of
        ValueKeyword _ txt -> const $ expand txt
        ParsedKeyword _ c -> const $ expand c
        _ -> ignore

affAttrExp :: Affiliated -> Expansions' HTag Attribute
affAttrExp aff =
  "affiliated" ## const . pure $ affAttrs aff

affAttrs :: Affiliated -> [(Text, Text)]
affAttrs aff = join $ mapMaybe getHtmlAttrs (toPairs aff)
  where
    getHtmlAttrs ("attr_html", BackendKeyword x) = Just x
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
    pure [rawNode $ T.replicate n "&nbsp;"]
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
    callExpansion "org:code" emptyNode
    `binding` contents txt
  (Verbatim txt) ->
    callExpansion "org:verbatim" emptyNode
    `binding` contents txt
  (Timestamp tsdata) ->
    timestamp tsdata
  (Entity name) ->
    expand (maybe "⍰" utf8Replacement (lookup name defaultEntitiesMap))
  (LaTeXFragment k c) ->
    latexFragment k c
  (ExportSnippet "html" c) ->
    pure [rawNode c]
  ExportSnippet {} ->
    pure []
  (FootnoteRef label) ->
    callExpansion "org:footnote-ref" emptyNode
    `binding` footnoteRef label
  (Cite _) -> expand ("(unresolved citation)" :: Text)
  InlBabelCall {} ->
    pure []
  (Src lang _ txt) ->
    callExpansion "org:src" emptyNode
    `binding` do
      "language" ## const $ expand lang
      contents txt
  (Link tgt inl) ->
    callExpansion "org:link" emptyNode
    `binding` (target tgt <> contents inl)
  (Image tgt) ->
    callExpansion "org:image" emptyNode `binding` (target tgt)
  (Target uid) ->
    pure [X.Element "a" [("id", uid)] []]
  Macro {} ->
    pure []

expandOrgElement :: OrgElement -> Expanded
expandOrgElement = \case
  (Paragraph aff [Image tgt]) ->
    callExpansion "org:figure" emptyNode
    `binding` affAttrExp aff
    `binding` (kwExps aff "kw:" <> target tgt)
  (Paragraph aff c) ->
    callExpansion "org:paragraph" emptyNode
    `binding` affAttrExp aff
    `binding` contents c
  (GreaterBlock aff Quote c) ->
    callExpansion "org:quote-block" emptyNode
    `binding` affAttrExp aff
    `binding` contents c
  (GreaterBlock aff Center c) ->
    callExpansion "org:center-block" emptyNode
    `binding` affAttrExp aff
    `binding` contents c
  (GreaterBlock aff (Special cls) c) ->
    callExpansion "org:special-block" emptyNode
    `binding` affAttrExp aff
    `binding` do
      "special-name" ## const $ expand cls
      contents c
  (PlainList aff k i) -> plainList k i
    `binding` affAttrExp aff
  (DynamicBlock _ _ els) -> expand els
  (Drawer _ els) -> expand els
  (ExportBlock "html" c) -> pure [rawNode c]
  (ExportBlock _ _) -> pure []
  (ExampleBlock aff st c) ->
    callExpansion "org:example-block" emptyNode
    `binding` affAttrExp aff
    `binding` srcOrExample st c
  (SrcBlock aff lang st _ c) ->
    callExpansion "org:src-block" emptyNode
    `binding` affAttrExp aff
    `binding` do
       "language" ## const $ expand lang
       srcOrExample st c
  (LaTeXEnvironment aff _ text) ->
    callExpansion "org:latex-environment" emptyNode
    `binding` affAttrExp aff
    `binding` contents text
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
  callExpansion "org:plain-list" emptyNode
  `binding` do
    "list-items" ## listItems
    "bullet" ## const bullet
    "counter" ## const counter
    switchCases plainListTag
  where
    listItems :: HTMLExpansion
    listItems inner =
      join <$> forM items \(ListItem _ i cbox t c) ->
        X.childNodes <$> inner
        `binding` do
          "counter-set" ## maybe ignore counterSet i
          "checkbox" ## maybe ignore checkbox cbox
          "tag" ## const $ expand t
          "contents" ## const $ expand $ cropRenderElements c
      where
        counterSet i _ = expand (show i :: Text)

        cropRenderElements (Paragraph _ objs : rest) = Left (objs, rest)
        cropRenderElements els = Right els

        checkbox :: Checkbox -> HTMLExpansion
        checkbox (BoolBox True) _ = callExpansion "checkbox:true" emptyNode
        checkbox (BoolBox False) _ = callExpansion "checkbox:false" emptyNode
        checkbox PartialBox  _ = callExpansion "checkbox:partial" emptyNode

    plainListTag = case kind of
      Ordered _   -> "ordered"
      Descriptive -> "descriptive"
      Unordered _ -> "unordered"

    bullet = case kind of
      Unordered b -> callExpansion ("bullet:" `T.snoc` b) emptyNode
      _ -> pure []

    counter = case kind of
      Ordered OrderedNum -> callExpansion "counter:num" emptyNode
      Ordered OrderedAlpha -> callExpansion "counter:alpha" emptyNode
      _ -> pure []

srcOrExample :: Maybe Int -> [SrcLine] -> HTMLExpansions
srcOrExample stNumber lins = do
  "src-lines" ## runLines
  where
    runLines :: HTMLExpansion
    runLines inner =
      intersperse (X.TextNode "\n") <$>
      foldMapM (flip lineSplices inner) (zip [0..] lins)

    lineNumber offset = do
      "line-number" ##
        justOrIgnore ((offset +) <$> stNumber) \n x ->
          X.childNodes <$> x
          `binding` do "number" ## const $ expand (show n :: Text)

    lineSplices (offset, SrcLine c) inner = switch "plain" inner
      `binding` do
        lineNumber offset
        contents c

    lineSplices (offset, RefLine i ref c) inner = switch "ref" inner
      `binding` do
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
    callExpansion "org:timestamp" emptyNode
    `binding` do
      dtExps d t r w
      switchCases (active a <> "-single")
  TimestampRange a (dateToDay -> d1, fmap toTime -> t1, r1, w1)
                   (dateToDay -> d2, fmap toTime -> t2, r2, w2) ->
    callExpansion "org:timestamp" emptyNode
    `binding` do
      "from" ## \x -> X.childNodes <$> x `binding` dtExps d1 t1 r1 w1
      "to"   ## \x -> X.childNodes <$> x `binding` dtExps d2 t2 r2 w2
      switchCases (active a <> "-range")
  where
    dtExps d t r w = do
      "repeater" ##
        justOrIgnore r \r' x -> X.childNodes <$> x `binding` tsMark r'
      "warning-period" ##
        justOrIgnore w \w' x -> X.childNodes <$> x `binding` tsMark w'
      tsDate d
      tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> HTMLExpansions
    tsMark (_,v,c) = do
      "value" ## const $ expand (show v :: Text)
      "unit" ## const $ callExpansion ("unit:" `T.snoc` c) emptyNode

    dateToDay (y,m,d,_) = fromGregorian (toInteger y) m d
    toTime (h,m) = TimeOfDay h m 0

    tsDate :: Day -> HTMLExpansions
    tsDate day = "ts-date" ## \input' -> do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ X.nodeText input
      pure . one . X.TextNode . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> HTMLExpansions
    tsTime time = "ts-time" ## \input' -> do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ X.nodeText input
      maybe (pure []) (pure . one . X.TextNode . toText . formatTime locale format) time

latexFragment :: FragmentType -> Text -> Expanded
latexFragment kind txt =
  callExpansion "org:latex-fragment" emptyNode
  `binding` do
    contents txt
    switchCases $
      case kind of
        InlMathFragment -> "inline"
        DispMathFragment -> "display"
        RawFragment -> "raw"
