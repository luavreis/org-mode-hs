-- |

module Org.Exporters.Heist
  ( loadOrgTemplates
  , renderSpliceable
  , renderSpliceableIO
  , renderHtml
  , renderHtmlWith
  ) where
import Data.Map.Syntax ((##))
import Heist
import Heist.Interpreted
import Org.Types
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime
import Data.Time.Format
import Org.Data.Entities (defaultEntitiesMap, htmlReplacement)
import Org.Exporters.Citeproc ()
import qualified Data.Text as T
import qualified Text.XmlHtml as X
import Relude.Extra (lookup, insert, toPairs)
import Data.ByteString.Builder (toLazyByteString)
import System.FilePath (normalise, (</>))
import Heist.Splices (ignoreImpl, ignoreTag, bindTag, bindImpl)
import Data.Char (isSpace)
import Paths_org_parser

callTemplate' :: Monad n => ByteString -> Splices (Splice n) -> Splice n
callTemplate' t = fmap trimSpaces . callTemplate t

quasiTrim :: Text -> Text
quasiTrim = T.foldr go ""
  where
    go c1 t@(T.uncons -> Just (c2, tr))
      | c1 == '\n', isSpace c2 = T.cons c1 tr
      | isSpace c1, isSpace c2 = t
    go c1 t = T.cons c1 t

trimSpaces :: Template -> Template
trimSpaces tpl = foldr go [] tpl
               & changeLast (mapText T.stripEnd)
               & changeFst  (mapText T.stripStart)
  where
    changeLast _ [] = []
    changeLast f [x] = [f x]
    changeLast f (x:xs) = x : changeLast f xs

    changeFst _ [] = []
    changeFst f (x:xs) = f x : xs

    mapText f (X.TextNode t) = X.TextNode (f t)
    mapText _ x = x

    go (X.TextNode t1) (uncons -> Just (X.TextNode t2, tr)) =
      X.TextNode (quasiTrim (t1 <> t2)) : tr
    go (X.TextNode t) r = X.TextNode (quasiTrim t) : r
    go (X.Comment _) r = r
    -- go (X.Element n a c) r | n /= "pre" = X.Element n a (trimSpaces c) : r
    go n r = n : r

defaultSplices :: Monad n => Splices (Splice n)
defaultSplices = do
  ignoreTag ## ignoreImpl
  bindTag ## bindImpl

loadOrgTemplates :: Monad n => HeistConfig n
loadOrgTemplates =
  let templateDir = loadTemplates . normalise =<< getDataDir
      elementsDir = loadTemplates . normalise . (</> "Elements") =<< getDataDir
      objectsDir = loadTemplates . normalise . (</> "Objects") =<< getDataDir
  in runIdentity $
    hcNamespace (const $ pure "") emptyHeistConfig
    >>= hcInterpretedSplices (const $ pure defaultSplices)
    >>= hcTemplateLocations (const $ pure [templateDir, elementsDir, objectsDir])

data ExporterState = ExporterState
  { footnoteCounter :: (Int, Map Text Int)
  , orgExportHeadlineLevels :: Int
  }

defaultExporterState :: ExporterState
defaultExporterState = ExporterState
  { footnoteCounter = (1, mempty)
  , orgExportHeadlineLevels = 3
  }

type MonadExporter n = MonadState ExporterState n

type Exporter = State ExporterState

getFootnoteRef :: MonadExporter n => Text -> n Text
getFootnoteRef label = do
  (i, m) <- gets footnoteCounter
  modify \s -> s { footnoteCounter = (i + 1, insert label i m) }
  pure (show i)

element :: Text -> [X.Node] -> [X.Node]
element name = one . X.Element name []

class Spliceable a where
  toSplice :: MonadExporter n => a -> Splice n

renderSpliceable :: Spliceable a => HeistState Exporter -> a -> ByteString
renderSpliceable st obj =
  evalHeistT (toSplice obj) (X.TextNode "") st
  & flip evalState defaultExporterState
  & X.renderXmlFragment X.UTF8
  & toStrict . toLazyByteString

instance Spliceable a => Spliceable [a] where
  toSplice = fmap join . mapM toSplice

instance (Spliceable a, Spliceable b) => Spliceable (Either a b) where
  toSplice = either toSplice toSplice

instance Spliceable Text where
  toSplice = textSplice

contents :: (Spliceable a, MonadExporter n) => a -> Splices (Splice n)
contents c = do
  "Contents" ## toSplice c

spliceOrEmpty :: Monad n => Maybe a -> (a -> Splice n) -> Splice n
spliceOrEmpty x f = maybe (pure []) f x

kwSplice :: MonadExporter n => Affiliated -> Text -> Splices (Splice n)
kwSplice kws name =
 name ## runChildrenWith $
  case lookup (T.toLower name) kws of
    Just (KeywordValue txt) -> contents txt
    Just (DualKeyword _ t) -> contents t
    Just (ParsedKeyword c) -> contents c
    Just (ParsedDualKeyword _ c) -> contents c
    _ -> mempty

-- * Document

renderSpliceableIO :: Spliceable a => a -> IO ByteString
renderSpliceableIO obj = do
  st <- initHeist loadOrgTemplates
  case st of
    Left e -> mapM putStrLn e *> error "WAAA"
    Right st' -> pure $ renderSpliceable st' obj

renderHtml :: ByteString -> OrgDocument -> IO ByteString
renderHtml tplname doc = do
  st <- initHeist loadOrgTemplates
  case st of
    Left e -> mapM putStrLn e *> error "WAAA"
    Right st' -> pure $ renderHtmlWith st' tplname doc

renderHtmlWith :: HeistState Exporter -> ByteString -> OrgDocument -> ByteString
renderHtmlWith hst tplname doc =
  callTemplate' tplname (documentSplices doc)
  & \x -> evalHeistT x (X.TextNode "") hst
  & flip evalState st'
  & X.renderXmlFragment X.UTF8
  & toStrict . toLazyByteString
  where
    st' = defaultExporterState -- todo: get options from the document

children :: MonadExporter n => [OrgSection] -> Splice n
children childs = do
  hlevels <- gets orgExportHeadlineLevels
  spliceOrEmpty (viaNonEmpty (sectionLevel . head) childs) \ level ->
    if level > hlevels
    then element "ol" <$> mapM (fmap (X.Element "li" []) . toSplice) childs
    else toSplice childs

tags :: MonadExporter n => Tag -> Splice n
tags tag = runChildrenWith $
  "Tag" ## toSplice tag

documentSplices :: MonadExporter n => OrgDocument -> Splices (Splice n)
documentSplices doc = do
  foldMap (kwSplice $ documentKeywords doc) ["Language", "Title", "Date", "Author"]
  contents (topLevelContents doc)
  "Sections" ## children (documentChildren doc)
  "Footnotes" ## do
    fnRefs <- snd <$> gets footnoteCounter -- TODO: is this run in the right order?? in the end?
    if not (null fnRefs)
    then
      runChildrenWith do
        "FootnoteDefs" ## join <$> forM (toPairs fnRefs) \ (fnLabel, i) ->
          spliceOrEmpty (lookup fnLabel (documentFootnotes doc)) \ fn ->
            runChildrenWith do
              "Number" ## textSplice (show i)
              contents fn
    else pure []

instance Spliceable OrgSection where
  toSplice section = do
    hlevels <- gets orgExportHeadlineLevels
    callTemplate' "Section" do
      "Headline" ## headline hlevels $ runChildrenWith do
        "TodoKw" ## spliceOrEmpty (sectionTodo section) todo
        "Priority" ## spliceOrEmpty (sectionPriority section) \p ->
          runChildrenWith $
            "Value" ## textSplice (priority p)
        "Title" ## toSplice $ sectionTitle section
        "Tags" ## join <$> forM (sectionTags section) tags
      "Anchor" ## textSplice (sectionAnchor section)
      contents (sectionContents section)
      "Subsections" ## children $ sectionChildren section
    where
      todo (TodoKeyword st nm) = runElementWith ("TodoKw:" <> todost st) $
        "TodoName" ## textSplice nm
      todost Done = "done"
      todost Todo = "todo"
      priority (LetterPriority c) = T.singleton c
      priority (NumericPriority n) = show n
      headline hlevels inner =
        if sectionLevel section > hlevels
        then (<> element "br" []) <$> inner
        else element ("h" <> show (sectionLevel section)) <$> inner

-- * Objects

instance Spliceable OrgInline where
  toSplice = renderOrgObject

renderOrgObject :: MonadExporter n => OrgInline -> Splice n
renderOrgObject = \case
  (Plain txt) -> toSplice txt
  SoftBreak -> textSplice " "
  LineBreak -> pure $ element "br" []
  (NBSpace n) -> toSplice (T.replicate n "&nbsp;")
  (Italic objs) -> element "i" <$> toSplice objs
  (Underline objs) -> element "u" <$> toSplice objs
  (Bold objs) -> element "b" <$> toSplice objs
  (Strikethrough objs) -> element "s" <$> toSplice objs
  (Superscript objs) -> element "sup" <$> toSplice objs
  (Subscript objs) -> element "sub" <$> toSplice objs
  (Quoted _ objs) -> element "q" <$> toSplice objs
  (Code txt) -> element "code" <$> toSplice txt
  (Verbatim txt) -> element "pre" <$> toSplice txt
  (Timestamp tsdata) -> callTemplate' "Timestamp" (timestamp tsdata)
  (Entity name) -> toSplice $ maybe "⍰" htmlReplacement (lookup name defaultEntitiesMap)
  (LaTeXFragment k c) -> callTemplate' "LaTeXFragment" (latexFragment k c)
  (ExportSnippet "html" c) -> toSplice c
  (ExportSnippet _ _) -> pure []
  (FootnoteRef label) -> callTemplate' "FootnoteRef" (footnoteRef label)
  (Cite cit) -> callTemplate' "Citation" (citation cit)

citation :: Citation -> Splices (Splice n)
citation = error "not implemented"

footnoteRef :: MonadExporter n => Text -> Splices (Splice n)
footnoteRef label = do
  "Counter" ## (toSplice =<< getFootnoteRef label)

timestamp :: forall n. MonadExporter n => TimestampData -> Splices (Splice n)
timestamp = \case
  TimestampData a (dateToDay -> d, fmap toTime -> t, r, w) -> do
    "Timestamp" ## runElement $ "Timestamp:" <> active a <> ":single"
    dtSplices d t r w
  TimestampRange a (dateToDay -> d1, fmap toTime -> t1, r1, w1)
                   (dateToDay -> d2, fmap toTime -> t2, r2, w2) -> do
    "Timestamp" ## runElement $ "Timestamp:" <> active a <> ":range"
    "From" ## runChildrenWith (dtSplices d1 t1 r1 w1)
    "To"   ## runChildrenWith (dtSplices d2 t2 r2 w2)
  where
    dtSplices d t r w = do
      "Repeater" ## spliceOrEmpty r (runChildrenWith . tsMark)
      "WarningPeriod" ## spliceOrEmpty w (runChildrenWith . tsMark)
      tsDate d
      tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> Splices (Splice n)
    tsMark (_,v,c) = do
      "Value" ## textSplice (show v)
      "Unit" ## runElement ("Unit:" `T.snoc` c)

    dateToDay (y,m,d,_) = fromGregorian (toInteger y) m d
    toTime (h,m) = TimeOfDay h m 0

    tsDate :: Day -> Splices (Splice n)
    tsDate day = "TSDate" ## do
      input <- getParamNode
      let dtl = defaultTimeLocale
          getAttr d s = maybe d separate $ X.getAttribute s input
          wdays'   = getAttr (map fst (wDays dtl)) "weekdays"
          swdays'  = getAttr (map snd (wDays dtl)) "shortweekdays"
          months'  = getAttr (map fst (months dtl)) "months"
          smonths' = getAttr (map snd (months dtl)) "shortmonths"
          locale = dtl { wDays = zip wdays' swdays'
                       , months = zip months' smonths'
                       }
          format = toString $ X.nodeText input
      toSplice . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> Splices (Splice n)
    tsTime time = "TSTime" ## do
      input <- getParamNode
      let dtl = defaultTimeLocale
          ampm' = X.getAttribute "ampm" input >>=
                  (\case [x,y] -> Just (x,y); _ -> Nothing) . separate
          locale = dtl { amPm = fromMaybe (amPm dtl) ampm' }
          format = toString $ X.nodeText input
      spliceOrEmpty time $ textSplice . toText . formatTime locale format

    separate = map (toString . T.strip) . T.split (==',')

latexFragment :: MonadExporter n => FragmentType -> Text -> Splices (Splice n)
latexFragment kind txt = do
  contents txt
  "LaTeXFragment" ##
    case kind of
      InlMathFragment -> runElement "LaTeXFragment:inline"
      DispMathFragment -> runElement "LaTeXFragment:display"
      RawFragment -> runElement "LaTeXFragment:raw"

-- * Elements

instance Spliceable OrgElement where
  toSplice = renderOrgElement

affSplices :: MonadExporter n => Affiliated -> Splices (Splice n)
affSplices aff = "WithAffiliated" ## do
  childs <- X.childNodes <$> getParamNode
  let children' = flip map childs \case
                    X.Element tag attrs childs' ->
                      X.Element tag (attrs <> affAttrs aff) childs'
                    x -> x
  localHS (bindSplices $ kwSplice aff "Caption") $
    runNodeList children'

affAttrs :: Affiliated -> [(Text, Text)]
affAttrs aff = join $ mapMaybe getHtmlAttrs (toPairs aff)
  where
    getHtmlAttrs ("attr_html", BackendKeyword x) = Just x
    getHtmlAttrs _ = Nothing

renderOrgElement :: MonadExporter n => OrgElement -> Splice n
renderOrgElement = \case
  (Paragraph aff c) -> one . X.Element "p" (affAttrs aff) <$> toSplice c
  (GreaterBlock aff Quote c) -> callTemplate' "QuoteBlock" (affSplices aff <> contents c)
  (GreaterBlock aff Center c) -> callTemplate' "CenterBlock" (affSplices aff <> contents c)
  (GreaterBlock aff (Special cls) c) -> callTemplate' "SpecialBlock" do
    affSplices aff
    "SpecialName" ## toSplice cls
    contents c
  (PlainList aff k i) -> callTemplate' "PlainList" (affSplices aff <> plainList k i)
  (DynamicBlock _ _ els) -> toSplice els
  (Drawer els) -> toSplice els
  -- Table {} -> error "Table html export is not yet implemented :( please help"
  (ExportBlock "html" c) -> toSplice c
  (ExportBlock _ _) -> pure []
  (ExampleBlock aff st c) -> callTemplate' "ExampleBlock" (affSplices aff <> srcOrExample st c)
  (SrcBlock aff lang st _ c) -> callTemplate' "SrcBlock" do
    "Language" ## toSplice lang
    affSplices aff
    srcOrExample st c
  HorizontalRule -> pure $ element "hr" []
  Keyword {} -> pure []

runElement :: Monad n => Text -> Splice n
runElement txt = runNode (X.Element txt [] [])

runElementWith :: Monad n => Text -> Splices (Splice n) -> Splice n
runElementWith txt spl = localHS (bindSplices spl) $ runElement txt

srcOrExample :: MonadExporter n => Maybe Int -> [SrcLine] -> Splices (Splice n)
srcOrExample stNumber lins = do
  "SrcLines" ## runLines
  "LineNumberMarker" ## spliceOrEmpty stNumber \n ->
    runChildrenWith ("Number" ## textSplice (show n))
  where
    runLines = join <$> forM lins lineSplices
    lineSplices (SrcLine c) = runElementWith "SrcLine:plain" $ contents c
    lineSplices (RefLine i ref c) = runElementWith "SrcLine:ref" do
      "Ref" ## toSplice ref
      "Id"  ## toSplice i
      contents c

plainList :: forall n. MonadExporter n => ListType -> [ListItem] -> Splices (Splice n)
plainList kind items = do
  "PlainList" ## runPlainList
  "ListItems" ## listItems
  "Bullet" ## bullet
  "Counter" ## counter
  where
    listItems =
      join <$> forM items \(ListItem _ i cbox t c) ->
        runChildrenWith do
          "Counter" ## spliceOrEmpty i counterSet
          "CheckBox" ## spliceOrEmpty cbox checkbox
          "Tag" ## toSplice t
          contents (cropRenderElements c)
      where
        counterSet :: Int -> Splice n
        counterSet i = pure $ (one . X.TextNode . show) i

        cropRenderElements [Paragraph _ objs] = Left objs
        cropRenderElements els = Right els

        checkbox :: Checkbox -> Splice n
        checkbox (BoolBox True) = runElement "Checkbox:true"
        checkbox (BoolBox False) = runElement "Checkbox:false"
        checkbox PartialBox = runElement "Checkbox:partial"

    runPlainList = case kind of
      Ordered _   -> runElement "PlainList:ordered"
      Descriptive -> runElement "PlainList:descriptive"
      Unordered _ -> runElement "PlainList:unordered"

    bullet = case kind of
      Unordered b -> runElement ("Bullet:" `T.snoc` b)
      _ -> pure []

    counter = case kind of
      Ordered OrderedNum -> runElement "Counter:num"
      Ordered OrderedAlpha -> runElement "Counter:alpha"
      _ -> pure []
