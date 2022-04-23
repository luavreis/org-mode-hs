{-# LANGUAGE FlexibleInstances #-}
-- |

module Org.Exporters.Heist
  ( loadOrgTemplates
  , renderSpliceable
  , renderSpliceableToDoc
  , renderSpliceableIO
  , renderHtml
  , renderHtmlIO
  , renderXml
  , renderXmlSpliceable
  , documentSplices
  , walkNodes
  , Exporter
  , Spliceable
  , toSplice
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
import System.FilePath (normalise, (</>), isRelative, takeExtension, (-<.>))
import Heist.Splices (ignoreImpl, ignoreTag, bindTag, bindImpl, applyTag, applyImpl)
import Data.ByteString.Lazy.Search (replace)
import Paths_org_parser

callTemplate' :: Monad n => ByteString -> Splices (Splice n) -> Splice n
callTemplate' name = fmap (trimSpaces False) . callTemplate name

walkNodes :: (X.Node -> X.Node) -> [X.Node] -> [X.Node]
walkNodes f = map f'
  where
    f' (X.Element name attr child) = f $ X.Element name attr (walkNodes f child)
    f' x = f x

trimSpaces :: Bool -> Template -> Template
trimSpaces recursive tpl = foldr go [] tpl
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
      X.TextNode (t1 <> t2) : tr
    go (X.Comment _) r = r
    go (X.Element n a c) r | recursive, n /= "pre", n /= "code" = X.Element n a (trimSpaces True c) : r
    go n r = n : r

defaultSplices :: Monad n => Splices (Splice n)
defaultSplices = do
  ignoreTag ## ignoreImpl
  bindTag ## bindImpl
  applyTag ## applyImpl

caseSplice :: Monad n => Text -> Splice n -> Splice n
caseSplice caseName splice = getCase =<< getParamNode
  where
    getCase (X.Element name attr child) =
      let
        cases :: Map Text X.Node
        cases = fromList $ flip mapMaybe child \case
          el@(X.Element "case" _ child') -> do
            tag <- X.getAttribute "tag" el
            pure (tag, X.Element name attr child')
          _ -> Nothing
      in maybe (pure []) (flip localParamNode splice . const) (lookup caseName cases)
    getCase _ = pure []

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
  toSplice :: a -> Splice Exporter

instance Spliceable (Splice Exporter) where
  toSplice x = x

instance {-# OVERLAPPABLE #-} Spliceable a => Spliceable [a] where
  toSplice = fmap join . mapM toSplice

instance (Spliceable a, Spliceable b) => Spliceable (Either a b) where
  toSplice = either toSplice toSplice

instance Spliceable Text where
  toSplice = textSplice

contents :: (Spliceable a) => a -> Splices (Splice Exporter)
contents c = do
  "Contents" ## toSplice c

spliceOrEmpty :: Monad n => Maybe a -> (a -> Splice n) -> Splice n
spliceOrEmpty x f = maybe (pure []) f x

kwSplice :: Affiliated -> Text -> Splices (Splice Exporter)
kwSplice kws name =
 name ##
  case lookup (T.toLower name) kws of
    Just (ValueKeyword _ txt) -> toSplice txt
    Just (ParsedKeyword _ c) -> toSplice c
    _ -> pure []

-- * Document

removeToBeRemoved :: LByteString -> LByteString
removeToBeRemoved =
  replace "</ToBeRemoved>" ("" :: LByteString) .
  replace "<ToBeRemoved xmlhtmlRaw>" ("" :: LByteString)

renderSpliceableToDoc :: Spliceable a => HeistState Exporter -> a -> LByteString
renderSpliceableToDoc st obj =
  evalHeistT (toSplice obj) (X.TextNode "") st
  & flip evalState defaultExporterState
  & trimSpaces True
  & X.HtmlDocument X.UTF8 (Just $ X.DocType "html" X.NoExternalID X.NoInternalSubset)
  & X.render
  & toLazyByteString
  & removeToBeRemoved

renderSpliceable :: Spliceable a => HeistState Exporter -> a -> LByteString
renderSpliceable st obj =
  evalHeistT (toSplice obj) (X.TextNode "") st
  & flip evalState defaultExporterState
  & trimSpaces True
  & X.renderHtmlFragment X.UTF8
  & toLazyByteString
  & removeToBeRemoved

renderSpliceableIO :: Spliceable a => a -> IO (Either [String] LByteString)
renderSpliceableIO obj =
  initHeist loadOrgTemplates
  <&> second (`renderSpliceable` obj)

renderHtmlIO :: ByteString -> OrgDocument -> IO (Either [String] LByteString)
renderHtmlIO tplname doc =
  initHeist loadOrgTemplates
  <&> second (\st' -> renderHtml st' tplname doc)

renderHtml :: HeistState Exporter -> ByteString -> OrgDocument -> LByteString
renderHtml hst tplname doc =
  callTemplate tplname (documentSplices doc)
  & \x -> evalHeistT x (X.TextNode "") hst
  & flip evalState st'
  & trimSpaces True
  & X.HtmlDocument X.UTF8 (Just $ X.DocType "html" X.NoExternalID X.NoInternalSubset)
  & X.render
  & toLazyByteString
  & removeToBeRemoved
  where
    st' = defaultExporterState -- todo: get options from the document

renderXml :: HeistState Exporter -> ByteString -> OrgDocument -> [X.Node]
renderXml hst tplname doc =
  callTemplate tplname (documentSplices doc)
  & \x -> evalHeistT x (X.TextNode "") hst
  & flip evalState st'
  where
    st' = defaultExporterState -- todo: get options from the document

renderXmlSpliceable :: HeistState Exporter -> ByteString -> OrgDocument -> [X.Node]
renderXmlSpliceable hst tplname doc =
  callTemplate tplname (documentSplices doc)
  & \x -> evalHeistT x (X.TextNode "") hst
  & flip evalState st'
  where
    st' = defaultExporterState -- todo: get options from the document

tags :: Tag -> Splice Exporter
tags tag = runChildrenWith $
  "Tag" ## toSplice tag

documentSplices :: OrgDocument -> Splices (Splice Exporter)
documentSplices doc = do
  foldMap (kwSplice $ keywordsFromList $ documentKeywords doc)
          ["Language", "Title", "Date", "Author"]
  contents (documentContent doc)
  "Footnotes" ## do
    fnRefs <- snd <$> gets footnoteCounter -- TODO: is this run in the right order?? in the end?
    if not (null fnRefs)
    then
      callTemplate' "Footnotes" do
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
      contents (sectionContent section)
    where
      todo (TodoKeyword st nm) = caseSplice (todost st) $
        runChildrenWith $
          "TodoName" ## textSplice nm
      todost Done = "done"
      todost Todo = "todo"
      priority (LetterPriority c) = T.singleton c
      priority (NumericPriority n) = show n
      headline hlevels inner =
        if sectionLevel section > hlevels
        then (<> element "br" []) <$> inner
        else element ("h" <> show (sectionLevel section)) <$> inner

-- * Contents

instance Spliceable OrgContent where
  toSplice content = do
    liftA2 (<>) (toSplice (fst content)) (children $ snd content)

instance {-# OVERLAPPABLE #-} (Spliceable a, Spliceable b) => Spliceable (a, b) where
  toSplice content = do
    liftA2 (<>) (toSplice (fst content)) (toSplice (snd content))

children :: [OrgSection] -> Splice Exporter
children childs = do
  hlevels <- gets orgExportHeadlineLevels
  spliceOrEmpty (viaNonEmpty (sectionLevel . head) childs) \ level ->
    if level > hlevels
    then element "ol" <$> mapM (fmap (X.Element "li" []) . toSplice) childs
    else toSplice childs

-- * Objects

instance Spliceable OrgInline where
  toSplice = renderOrgObject

renderOrgObject :: OrgInline -> Splice Exporter
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
  (Code txt) -> one . X.Element "code" [("class", "code")] <$> toSplice txt
  (Verbatim txt) -> one . X.Element "code" [("class", "verbatim")] <$> toSplice txt
  (Timestamp tsdata) -> callTemplate' "Timestamp" (timestamp tsdata)
  (Entity name) -> rawEl <$> toSplice (maybe "⍰" htmlReplacement (lookup name defaultEntitiesMap))
  (LaTeXFragment k c) -> callTemplate' "LaTeXFragment" (latexFragment k c)
  (ExportSnippet "html" c) -> rawEl <$> toSplice c
  ExportSnippet {} -> pure []
  (FootnoteRef label) -> callTemplate' "FootnoteRef" (footnoteRef label)
  (Cite cit) -> callTemplate' "Citation" (citation cit)
  InlBabelCall {} -> pure []
  (Src lang _ txt) -> one . X.Element "code" [("class", "src " <> lang)] <$> toSplice txt
  (Link tgt inl) -> callTemplate' "Link" (target tgt <> contents inl)
  (Image tgt) -> callTemplate' "Image" (target tgt)
  Macro {} -> pure []
  where
    rawEl = one . X.Element "ToBeRemoved" [("xmlhtmlRaw", "")]

target :: LinkTarget -> Splices (Splice Exporter)
target tgt = do
  "Target" ## textSplice $ case tgt of
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


citation :: Citation -> Splices (Splice n)
citation = error "not implemented"

footnoteRef :: Text -> Splices (Splice Exporter)
footnoteRef label = do
  "Counter" ## (toSplice =<< getFootnoteRef label)

timestamp :: TimestampData -> Splices (Splice Exporter)
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

    tsMark :: TimestampMark -> Splices (Splice Exporter)
    tsMark (_,v,c) = do
      "Value" ## textSplice (show v)
      "Unit" ## runElement ("Unit:" `T.snoc` c)

    dateToDay (y,m,d,_) = fromGregorian (toInteger y) m d
    toTime (h,m) = TimeOfDay h m 0

    tsDate :: Day -> Splices (Splice Exporter)
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

    tsTime :: Maybe TimeOfDay -> Splices (Splice Exporter)
    tsTime time = "TSTime" ## do
      input <- getParamNode
      let dtl = defaultTimeLocale
          ampm' = X.getAttribute "ampm" input >>=
                  (\case [x,y] -> Just (x,y); _ -> Nothing) . separate
          locale = dtl { amPm = fromMaybe (amPm dtl) ampm' }
          format = toString $ X.nodeText input
      spliceOrEmpty time $ textSplice . toText . formatTime locale format

    separate = map (toString . T.strip) . T.split (==',')

latexFragment :: FragmentType -> Text -> Splices (Splice Exporter)
latexFragment kind txt = do
  "FragmentContents" ## textSplice txt
  "LaTeXFragment" ##
    case kind of
      InlMathFragment -> runElement "LaTeXFragment:inline"
      DispMathFragment -> runElement "LaTeXFragment:display"
      RawFragment -> runElement "LaTeXFragment:raw"

-- * Elements

instance Spliceable OrgElement where
  toSplice = renderOrgElement

affSplices :: Affiliated -> Splices (Splice Exporter)
affSplices aff = "WithAffiliated" ## do
  childs <- runChildrenWith $ kwSplice aff "Caption"
  pure $ flip map childs \case
    X.Element tag attrs childs' ->
      X.Element tag (attrs <> affAttrs aff) childs'
    x -> x

affAttrs :: Affiliated -> [(Text, Text)]
affAttrs aff = join $ mapMaybe getHtmlAttrs (toPairs aff)
  where
    getHtmlAttrs ("attr_html", BackendKeyword x) = Just x
    getHtmlAttrs _ = Nothing

renderOrgElement :: OrgElement -> Splice Exporter
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
  (Drawer _ els) -> toSplice els
  -- Table {} -> error "Table html export is not yet implemented :( please help"
  (ExportBlock "html" c) -> one . X.Element "ToBeRemoved" [("xmlhtmlRaw", "")] <$> toSplice c
  (ExportBlock _ _) -> pure []
  (ExampleBlock aff st c) -> callTemplate' "ExampleBlock" (affSplices aff <> srcOrExample st c)
  (SrcBlock aff lang st _ c) -> callTemplate' "SrcBlock" do
    "Language" ## toSplice lang
    affSplices aff
    srcOrExample st c
  (LaTeXEnvironment aff text) -> callTemplate' "LaTeXEnvironment" (affSplices aff <> contents text)
  HorizontalRule -> pure $ element "hr" []
  Keyword {} -> pure []

runElement :: Monad n => Text -> Splice n
runElement txt = runNode (X.Element txt [] [])

srcOrExample :: Maybe Int -> [SrcLine] -> Splices (Splice Exporter)
srcOrExample stNumber lins = do
  "SrcLines" ## runLines
  where
    runLines = intercalate [X.TextNode "\n"] <$>
               forM (zip [0..] lins) (fmap (trimSpaces True) . lineSplices)
    lineNumber offset = do
      "LineNumber" ##
        spliceOrEmpty ((offset +) <$> stNumber) \n ->
          runChildrenWith ("Number" ## textSplice (show n))
    lineSplices (offset, SrcLine c) = caseSplice "plain" $
      runChildrenWith do
        lineNumber offset
        contents c
    lineSplices (offset, RefLine i ref c) = caseSplice "ref" $
      runChildrenWith do
        lineNumber offset
        "Ref" ## toSplice ref
        "Id"  ## toSplice i
        contents c

plainList :: ListType -> [ListItem] -> Splices (Splice Exporter)
plainList kind items = do
  "PlainList" ## runPlainList
  "ListItems" ## listItems
  "Bullet" ## bullet
  "Counter" ## counter
  where
    listItems =
      join <$> forM items \(ListItem _ i cbox t c) ->
        runChildrenWith do
          "CounterSet" ## spliceOrEmpty i counterSet
          "Checkbox" ## spliceOrEmpty cbox checkbox
          "Tag" ## toSplice t
          contents (cropRenderElements c)
      where
        counterSet :: Int -> Splice Exporter
        counterSet i = pure $ (one . X.TextNode . show) i

        cropRenderElements (Paragraph _ objs : rest) = Left (objs, rest)
        cropRenderElements els = Right els

        checkbox :: Checkbox -> Splice Exporter
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
