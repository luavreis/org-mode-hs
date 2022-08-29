{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Org.Exporters.Common where

import Data.Map.Syntax
import Data.Text qualified as T
import Data.Time (Day, TimeLocale, TimeOfDay (..), defaultTimeLocale, formatTime, fromGregorian)
import Ondim
import Ondim.Extra (Attribute, HasAttrChild, ifElse, ignore, switch, switchCases)
import Org.Data.Entities qualified as Data
import Org.Types
import Org.Walk
import Paths_org_exporters (getDataDir)
import Relude.Extra (insert, lookup, toPairs)
import System.FilePath (isRelative, takeExtension, (-<.>), (</>))

templateDir :: IO FilePath
templateDir = (</> "templates") <$> getDataDir

data ExporterState = ExporterState
  { footnoteCounter :: (Int, Map Text Int),
    allFootnotes :: Map Text [OrgElement],
    exporterSettings :: ExporterSettings
  }

defaultExporterState :: ExporterState
defaultExporterState =
  ExporterState
    { footnoteCounter = (1, mempty),
      allFootnotes = mempty,
      exporterSettings = defaultExporterSettings
    }

getSetting :: MonadExporter m => (ExporterSettings -> b) -> m b
getSetting f = f <$> gets exporterSettings

getFootnoteRef :: MonadExporter n => Text -> n (Maybe (Text, [OrgElement]))
getFootnoteRef label =
  gets (lookup label . allFootnotes) >>= \case
    Just els -> do
      (i, m) <- gets footnoteCounter
      case lookup label m of
        Just num ->
          pure $ Just (show num, els)
        Nothing -> do
          modify \s -> s {footnoteCounter = (i + 1, insert label i m)}
          pure $ Just (show i, els)
    Nothing -> pure Nothing

type MonadExporter n = MonadState ExporterState n

data ExporterSettings = ExporterSettings
  { -- | The last level which is still exported as a headline.
    --
    -- Inferior levels will usually produce itemize or enumerate lists
    -- when exported, but back-end behavior may differ.
    --
    -- This option can also be set with the OPTIONS keyword,
    -- e.g. "H:2".
    orgExportHeadlineLevels :: Int,
    -- | Interpret "\-", "--", "---" and "..." for export.
    --
    -- This option can also be set with the OPTIONS keyword,
    -- e.g. "-:nil".
    orgExportWithSpecialStrings :: Bool,
    -- | Interpret entities when exporting.
    --
    -- This option can also be set with the OPTIONS keyword,
    -- e.g. "e:nil".
    orgExportWithEntities :: Bool,
    -- | Global shift of headline levels.
    headlineLevelShift :: Int,
    timeLocale :: TimeLocale
  }
  deriving (Eq, Show)

defaultExporterSettings :: ExporterSettings
defaultExporterSettings =
  ExporterSettings
    { orgExportHeadlineLevels = 3,
      orgExportWithSpecialStrings = True,
      orgExportWithEntities = True,
      headlineLevelShift = 0,
      timeLocale = defaultTimeLocale
    }

doSpecialStrings :: Text -> Text
doSpecialStrings txt =
  txt & T.replace "---" "—"
    & T.replace "--" "–"
    & T.replace "..." "…"
    & T.replace "\\-" "\173"

processSpecialStrings :: Walkable OrgObject a => a -> a
processSpecialStrings = walk process
  where
    process :: OrgObject -> OrgObject
    process (Plain txt) = Plain $ doSpecialStrings txt
    process x = x

justOrIgnore :: OndimTag tag => Maybe a -> (a -> Expansion tag b) -> Expansion tag b
justOrIgnore = flip (maybe ignore)

tags :: HasAttrChild tag t => Tag -> Expansion tag t
tags tag x = children x `bindingText` ("tag" ## pure tag)

parsedKwExpansions :: ExportBackend tag => Affiliated -> Text -> Expansions' tag (ObjectNode tag)
parsedKwExpansions kws prefix =
  forM_ parsedKws \(name, t) ->
    (prefix <> name) ## const $ expandOrgObjects t
  where
    parsedKws =
      mapMaybe
        (\case (n, ParsedKeyword _ t) -> Just (n, t); _ -> Nothing)
        (toPairs kws)

textKwExpansions :: OndimTag t => Affiliated -> Text -> MapSyntax Text (Ondim t Text)
textKwExpansions kws prefix =
  forM_ textKws \(name, t) ->
    (prefix <> name) ## pure t
  where
    textKws =
      mapMaybe
        (\case (n, ValueKeyword _ t) -> Just (n, t); _ -> Nothing)
        (toPairs kws)

-- | Attribute expansion for affiliated keywords.
affiliatedAttrExpansions :: OndimTag t => Text -> Affiliated -> Expansions' t Attribute
affiliatedAttrExpansions lang aff =
  "affiliated" ## const $ pure affAttrs
  where
    affAttrs :: [(Text, Text)]
    affAttrs = join $ mapMaybe getHtmlAttrs (toPairs aff)
      where
        getHtmlAttrs (k, BackendKeyword x)
          | ("attr_" <> lang) `T.isPrefixOf` k = Just x
        getHtmlAttrs _ = Nothing

-- | Text expansion for link target.
linkTarget :: OndimTag t => LinkTarget -> MapSyntax Text (Ondim t Text)
linkTarget tgt = do
  "target" ## pure case tgt of
    URILink "file" (changeExtension -> file)
      | isRelative file -> toText file
      | otherwise -> "file:///" <> T.dropWhile (== '/') (toText file)
    URILink protocol uri -> protocol <> ":" <> uri
    InternalLink anchor -> "#" <> anchor
    UnresolvedLink tgt' -> tgt'
  case tgt of
    URILink protocol _ -> "protocol" ## pure protocol
    InternalLink {} -> "protocol" ## pure "internal"
    _ -> pure ()
  where
    changeExtension (toString -> file) =
      if takeExtension file == ".org"
        then file -<.> ".html"
        else file

class
  ( HasAttrChild tag (ObjectNode tag),
    HasAttrChild tag (ElementNode tag),
    OndimNode tag (DocumentNode tag),
    MonadExporter (OndimMonad tag)
  ) =>
  ExportBackend tag
  where
  type ObjectNode tag
  nullObj :: ObjectNode tag
  plain :: Text -> [ObjectNode tag]
  softbreak :: [ObjectNode tag]
  exportSnippet :: Text -> Text -> [ObjectNode tag]
  type ElementNode tag
  nullEl :: ElementNode tag
  rawBlock :: Text -> Text -> [ElementNode tag]
  mergeLists :: Filter tag (ElementNode tag)
  mergeLists = id
  hN :: Int -> Expansion tag (ElementNode tag)
  plainObjsToEls :: [ObjectNode tag] -> [ElementNode tag]
  stringify :: ObjectNode tag -> Text
  type DocumentNode tag

expandOrgObjects :: ExportBackend tag => [OrgObject] -> Ondim tag [ObjectNode tag]
expandOrgObjects = foldMapM expandOrgObject

expandOrgObject ::
  forall tag.
  ExportBackend tag =>
  OrgObject ->
  Ondim tag [ObjectNode tag]
expandOrgObject obj =
  withText debugExps $
    case obj of
      (Plain txt) -> do
        specialStrings <- getSetting orgExportWithSpecialStrings
        pure $
          plain @tag
            (if specialStrings then doSpecialStrings txt else txt)
      SoftBreak ->
        pure $ softbreak @tag
      LineBreak ->
        call "org:object:linebreak"
      (Code txt) ->
        call "org:object:code"
          `bindingText` do "content" ## pure txt
      (Entity name) -> do
        withEntities <- getSetting orgExportWithEntities
        case lookup name Data.defaultEntitiesMap of
          Just (Data.Entity _ latex mathP html ascii latin utf8)
            | withEntities ->
                call "org:object:entity"
                  `binding` do
                    "entity:if-math" ## ifElse @(ObjectNode tag) mathP
                  `bindingText` do
                    "entity:latex" ## pure latex
                    "entity:ascii" ## pure ascii
                    "entity:html" ## pure html
                    "entity:latin" ## pure latin
                    "entity:utf8" ## pure utf8
          _ -> pure $ plain @tag ("\\" <> name)
      (LaTeXFragment ftype txt) ->
        call "org:object:latex-fragment"
          `bindingText` do
            "content" ## pure txt
          `binding` do
            switchCases @(ObjectNode tag) $
              case ftype of
                InlMathFragment -> "inline"
                DispMathFragment -> "display"
                RawFragment -> "raw"
      (ExportSnippet backend code) ->
        pure $ exportSnippet @tag backend code
      (Src lang _params txt) ->
        call "org:object:src"
          `bindingText` do
            "language" ## pure lang
            "content" ## pure txt
      (Target anchor) ->
        call "org:object:target"
          `bindingText` do
            "anchor" ## pure anchor
      (Italic objs) ->
        call "org:object:italic"
          `binding` do
            "content" ## expObjs objs
      (Underline objs) ->
        call "org:object:underline"
          `binding` do
            "content" ## expObjs objs
      (Bold objs) ->
        call "org:object:bold"
          `binding` do
            "content" ## expObjs objs
      (Strikethrough objs) ->
        call "org:object:strikethrough"
          `binding` do
            "content" ## expObjs objs
      (Superscript objs) ->
        call "org:object:superscript"
          `binding` do
            "content" ## expObjs objs
      (Subscript objs) ->
        call "org:object:subscript"
          `binding` do
            "content" ## expObjs objs
      (Quoted qtype objs) ->
        call "org:object:quoted"
          `binding` do
            "content" ## expObjs objs
            switchCases
              case qtype of
                SingleQuote -> "single"
                DoubleQuote -> "double"
      (Verbatim txt) ->
        call "org:object:verbatim"
          `bindingText` do
            "content" ## pure txt
      (Link tgt objs) ->
        call "org:object:link"
          `bindingText` linkTarget tgt
          `binding` do
            "content" ## expObjs objs
      (Image tgt) ->
        call "org:object:image"
          `bindingText` linkTarget tgt
      (Timestamp ts) ->
        timestamp ts
      (FootnoteRef name) -> do
        ref <- getFootnoteRef name
        call "org:object:footnote-ref"
          `binding` do
            whenJust ref \ ~(_, els) ->
              "footnote-ref:content" ## const $ expandOrgElements els
          `bindingText` do
            "footnote-ref:key" ## pure name
            whenJust ref \ ~(num, _) ->
              "footnote-ref:number" ## pure num
      (Cite _) ->
        pure $ plain @tag "(unresolved citation)"
      InlBabelCall {} ->
        error "TODO"
      Macro {} ->
        error "TODO"
  where
    debugExps =
      fromList
        [ ("debug:ast", pure (show obj))
        ]
    expObjs :: [OrgObject] -> Expansion tag (ObjectNode tag)
    expObjs o = const $ expandOrgObjects o
    call x = callExpansion x (pure (nullObj @tag))

expandOrgElements :: ExportBackend tag => [OrgElement] -> Ondim tag [ElementNode tag]
expandOrgElements = foldMapM expandOrgElement

expandOrgElement ::
  forall tag.
  ExportBackend tag =>
  OrgElement ->
  Ondim tag [ElementNode tag]
expandOrgElement el =
  withText debugExps $
    case el of
      (Paragraph aff [Image tgt]) ->
        call "org:element:figure"
          `bindingAff` aff
          `bindingText` linkTarget tgt
      (Paragraph aff c) ->
        call "org:element:paragraph"
          `bindingAff` aff
          `binding` ("content" ## const $ expandOrgObjects c)
      (GreaterBlock aff Quote c) ->
        call "org:element:quote-block"
          `bindingAff` aff
          `binding` do
            "content" ## expEls c
      (GreaterBlock aff Center c) ->
        call "org:element:center-block"
          `bindingAff` aff
          `binding` do
            "content" ## expEls c
      (GreaterBlock aff (Special cls) c) ->
        call "org:element:special-block"
          `bindingAff` aff
          `bindingText` do "special-name" ## pure cls
          `binding` do
            "content" ## expEls c
      (PlainList aff k i) ->
        plainList k i
          `bindingAff` aff
      (DynamicBlock _ _ els) ->
        expandOrgElements els
      (Drawer _ els) ->
        expandOrgElements els
      (ExportBlock lang code) ->
        pure $ rawBlock @tag lang code
      (ExampleBlock aff i c) ->
        srcOrExample "org:element:example-block" i c
          `bindingAff` aff
          `bindingText` do
            "content" ## pure $ T.intercalate "\n" (srcLineContent <$> c)
      (SrcBlock aff lang i _ c) ->
        srcOrExample "org:element:src-block" i c
          `bindingAff` aff
          `bindingText` do
            "language" ## pure lang
            "content" ## pure $ T.intercalate "\n" (srcLineContent <$> c)
      (LaTeXEnvironment aff _ text) ->
        call "org:element:latex-environment"
          `bindingAff` aff
          `bindingText` do "content" ## pure text
      HorizontalRule ->
        call "org:element:horizontal-rule"
      Keyword k (ValueKeyword _ v) ->
        call "org:element:keyword"
          `bindingText` do
            "keyword:key" ## pure k
            "keyword:value" ## pure v
          `binding` switchCases @(ElementNode tag) "keyword:textual"
      Keyword k (ParsedKeyword _ v) ->
        call "org:element:keyword"
          `bindingText` do
            "keyword:key" ## pure k
          `binding` switchCases @(ElementNode tag) "keyword:parsed"
          `binding` do
            "keyword:value" ## const $ expandOrgObjects v
      Keyword {} -> pure []
      VerseBlock {} ->
        error "TODO"
      Clock {} ->
        error "TODO"
  where
    debugExps =
      fromList
        [ ("debug:ast", pure (show el))
        ]
    bindingAff x aff =
      x
        `binding` affiliatedAttrExpansions "html" aff
        `binding` parsedKwExpansions aff "akw:"
        `bindingText` textKwExpansions aff "akw:"
    expEls :: [OrgElement] -> Expansion tag (ElementNode tag)
    expEls o = const $ expandOrgElements o
    call x = callExpansion x (pure (nullEl @tag))

expandOrgSections ::
  forall tag.
  ExportBackend tag =>
  [OrgSection] ->
  Ondim tag [ElementNode tag]
expandOrgSections [] = pure []
expandOrgSections sections@(OrgSection {sectionLevel = level} : _) = do
  hlevels <- getSetting orgExportHeadlineLevels
  shift <- getSetting headlineLevelShift
  callExpansion "org:sections" (pure $ nullEl @tag)
    `binding` do
      if level + shift > hlevels
        then switchCases @(ElementNode tag) "over-level"
        else switchCases @(ElementNode tag) "normal"
      "sections" ## \x ->
        mergeLists $
          join <$> forM sections \section ->
            children x
              `binding` do
                "section:headline"
                  ## const
                  $ toList <$> expandOrgObjects (sectionTitle section)
                "tags" ## \inner ->
                  join <$> forM (sectionTags section) (`tags` inner)
              `binding` do
                "section:children" ## const $ toList <$> expandOrgElements (sectionChildren section)
                "section:subsections" ## const $ expandOrgSections (sectionSubsections section)
                "h-n" ## hN (sectionLevel section + shift)
              `bindingText` do
                for_ (sectionTodo section) todo
                for_ (sectionPriority section) priority
                for_ (toPairs $ sectionProperties section) \(k, v) ->
                  "prop:" <> k ## pure v
                "anchor" ## pure $ sectionAnchor section
                -- Debug
                "debug:ast" ## pure (show section)
  where
    todo (TodoKeyword st nm) = do
      "todo-state" ## pure (todost st)
      "todo-name" ## pure nm
    todost Done = "done"
    todost Todo = "todo"
    priority p =
      "priority" ## pure case p of
        (LetterPriority c) -> T.singleton c
        (NumericPriority n) -> show n

liftDocument ::
  forall tag.
  ExportBackend tag =>
  OrgDocument ->
  DocumentNode tag ->
  Ondim tag (DocumentNode tag)
liftDocument doc node = do
  modify (\s -> s {allFootnotes = documentFootnotes doc})
  liftSubstructures node
    `binding` do
      parsedKwExpansions
        (keywordsFromList $ documentKeywords doc)
        "doc:kw:"
    `bindingText` do
      textKwExpansions
        (keywordsFromList $ documentKeywords doc)
        "doc:kw:"
      for_ (toPairs $ documentProperties doc) \(k, v) ->
        "doc:prop:" <> k ## pure v
    `binding` do
      "doc:children" ## const $ expandOrgElements (documentChildren doc)
      "doc:sections" ## const $ expandOrgSections (documentSections doc)
      "doc:footnotes" ## \node' -> do
        fns <-
          gets (toPairs . snd . footnoteCounter)
            <&> mapMaybe \(ref, num) ->
              (,num) <$> lookup ref (documentFootnotes doc)
        if not (null fns)
          then
            children node'
              `binding` do
                "footnote-defs" ## \inner ->
                  join <$> forM fns \(els, num) ->
                    children @(ElementNode tag) inner
                      `bindingText` do
                        "footnote-def:number" ## pure (show num)
                      `binding` do
                        "footnote-def:content" ## const $ expandOrgElements els
          else pure []

plainList ::
  forall tag.
  ExportBackend tag =>
  ListType ->
  [ListItem] ->
  Ondim tag [ElementNode tag]
plainList kind items =
  callExpansion "org:element:plain-list" (pure $ nullEl @tag)
    `binding` do
      "list-items" ## listItems
      case kind of
        Ordered OrderedNum -> switchCases "ordered-num"
        Ordered OrderedAlpha -> switchCases "ordered-alpha"
        Descriptive -> switchCases "descriptive"
        Unordered _ -> switchCases "unordered"
    `bindingText` case kind of
      Unordered b ->
        "bullet" ## pure (one b)
      _ -> mempty
  where
    listItems :: Expansion tag (ElementNode tag)
    listItems inner =
      mergeLists $
        join <$> forM items \(ListItem _ i cbox t c) ->
          children inner
            `bindingText` do
              "counter-set" ## pure $ maybe "" show i
              "checkbox" ## pure $ maybe "" checkbox cbox
            `binding` do
              "descriptive-tag" ## const $ expandOrgObjects t
            `binding` do
              "list-item-content" ## const $ doPlainOrPara c
      where
        doPlainOrPara :: [OrgElement] -> Ondim tag [ElementNode tag]
        doPlainOrPara [Paragraph _ objs] = plainObjsToEls @tag <$> (expandOrgObjects objs)
        doPlainOrPara els = expandOrgElements els

        _start = join $ flip viaNonEmpty items \(ListItem _ i _ _ _ :| _) -> i

        -- adjFstF :: Filter tag ElementNode tag
        -- adjFstF = (map go <$>)
        --   where
        --     go (P.OrderedList (n, y, z) b) = P.OrderedList (fromMaybe n start, y, z) b
        --     go b = b

        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox = "partial"

srcOrExample ::
  forall tag.
  ExportBackend tag =>
  Text ->
  Maybe Int ->
  [SrcLine] ->
  Ondim tag [ElementNode tag]
srcOrExample name stNumber lins =
  callExpansion name (pure $ nullEl @tag)
    `binding` ("src-lines" ## runLines)
    `bindingText` do
      whenJust stNumber \num ->
        "starting-number" ## pure $ show num
      "content" ## pure $ T.intercalate "\n" (srcLineContent <$> lins)
  where
    runLines :: Expansion tag (ObjectNode tag)
    runLines inner =
      intercalate (plain @tag "\n")
        <$> mapM (flip lineExps inner) (zip [0 ..] lins)

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
          "id" ## pure i
          "content" ## pure c

timestamp ::
  forall tag.
  ExportBackend tag =>
  TimestampData ->
  Ondim tag [ObjectNode tag]
timestamp ts =
  callExpansion "org:object:timestamp" (pure $ nullObj @tag)
    `binding` case ts of
      TimestampData a (dateToDay -> d, fmap toTime -> t, r, w) -> do
        dtExps d t r w
        switchCases (active a <> "-single")
      TimestampRange
        a
        (dateToDay -> d1, fmap toTime -> t1, r1, w1)
        (dateToDay -> d2, fmap toTime -> t2, r2, w2) -> do
          "from" ## \x -> children x `binding` dtExps d1 t1 r1 w1
          "to" ## \x -> children x `binding` dtExps d2 t2 r2 w2
          switchCases @(ObjectNode tag) (active a <> "-range")
  where
    dtExps d t r w = do
      "repeater"
        ## justOrIgnore r \r' x -> children x `bindingText` tsMark r'
      "warning-period"
        ## justOrIgnore w \w' x -> children x `bindingText` tsMark w'
      "ts-date" ## tsDate d
      "ts-time" ## tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> MapSyntax Text (Ondim tag Text)
    tsMark (_, v, c) = do
      "value" ## pure $ show v
      "unit" ## pure $ one c

    dateToDay (y, m, d, _) = fromGregorian (toInteger y) m d
    toTime (h, m) = TimeOfDay h m 0

    tsDate :: Day -> Expansion tag (ObjectNode tag)
    tsDate day input' = do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ stringify @tag input
      pure . plain @tag . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> Expansion tag (ObjectNode tag)
    tsTime time input' = do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ stringify @tag input
      maybe (pure []) (pure . plain @tag . toText . formatTime locale format) time
