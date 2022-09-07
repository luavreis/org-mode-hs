{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
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

parsedKwExpansions ::
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  Affiliated ->
  Text ->
  Expansions' tag obj
parsedKwExpansions bk kws prefix =
  forM_ parsedKws \(name, t) ->
    (prefix <> name) ## const $ expandOrgObjects bk t
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

type BackendC tag obj elm =
  ( HasAttrChild tag obj,
    HasAttrChild tag elm,
    MonadExporter (OndimMonad tag)
  )

data ExportBackend tag obj elm = ExportBackend
  { nullObj :: obj,
    plain :: Text -> [obj],
    softbreak :: [obj],
    exportSnippet :: Text -> Text -> [obj],
    nullEl :: elm,
    srcPretty :: Affiliated -> Text -> Text -> OndimMonad tag (Maybe [[obj]]),
    rawBlock :: Text -> Text -> [elm],
    mergeLists :: Filter tag (elm),
    hN :: Int -> Expansion tag (elm),
    plainObjsToEls :: [obj] -> [elm],
    stringify :: obj -> Text
  }

expandOrgObjects ::
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  [OrgObject] ->
  Ondim tag [obj]
expandOrgObjects = foldMapM . expandOrgObject

expandOrgObject ::
  forall tag obj elm.
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  OrgObject ->
  Ondim tag [obj]
expandOrgObject bk@(ExportBackend {..}) obj =
  withText debugExps $
    case obj of
      (Plain txt) -> do
        specialStrings <- getSetting orgExportWithSpecialStrings
        pure $
          plain (if specialStrings then doSpecialStrings txt else txt)
      SoftBreak ->
        pure $ softbreak
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
                    "entity:if-math" ## ifElse @obj mathP
                  `bindingText` do
                    "entity:latex" ## pure latex
                    "entity:ascii" ## pure ascii
                    "entity:html" ## pure html
                    "entity:latin" ## pure latin
                    "entity:utf8" ## pure utf8
          _ -> pure $ plain ("\\" <> name)
      (LaTeXFragment ftype txt) ->
        call "org:object:latex-fragment"
          `bindingText` do
            "content" ## pure txt
          `binding` do
            switchCases @obj $
              case ftype of
                InlMathFragment -> "inline"
                DispMathFragment -> "display"
                RawFragment -> "raw"
      (ExportSnippet backend code) ->
        pure $ exportSnippet backend code
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
          `bindingText` do
            linkTarget tgt
          `binding` do
            "content" ## expObjs objs
      (Image tgt) ->
        call "org:object:image"
          `bindingText` linkTarget tgt
      (Timestamp ts) ->
        timestamp bk ts
      (FootnoteRef name) -> do
        ref <- getFootnoteRef name
        call "org:object:footnote-ref"
          `binding` do
            whenJust ref \ ~(_, els) ->
              "footnote-ref:content" ## const $ expandOrgElements bk els
          `bindingText` do
            "footnote-ref:key" ## pure name
            whenJust ref \ ~(num, _) ->
              "footnote-ref:number" ## pure num
      (Cite _) ->
        pure $ plain "(unresolved citation)"
      InlBabelCall {} ->
        error "TODO"
      Macro {} ->
        error "TODO"
  where
    debugExps =
      fromList
        [ ("debug:ast", pure (show obj))
        ]
    expObjs :: [OrgObject] -> Expansion tag obj
    expObjs o = const $ expandOrgObjects bk o
    call x = callExpansion x (pure nullObj)

expandOrgElements ::
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  [OrgElement] ->
  Ondim tag [elm]
expandOrgElements = foldMapM . expandOrgElement

expandOrgElement ::
  forall tag obj elm.
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  OrgElement ->
  Ondim tag [elm]
expandOrgElement bk@(ExportBackend {..}) el =
  withText debugExps $
    case el of
      (Paragraph aff [Image tgt]) ->
        call "org:element:figure"
          `bindingAff` aff
          `bindingText` linkTarget tgt
      (Paragraph aff c) ->
        call "org:element:paragraph"
          `bindingAff` aff
          `binding` ("content" ## const $ expandOrgObjects bk c)
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
        plainList bk k i
          `bindingAff` aff
      (DynamicBlock _ _ els) ->
        expandOrgElements bk els
      (Drawer _ els) ->
        expandOrgElements bk els
      (ExportBlock lang code) ->
        pure $ rawBlock lang code
      (ExampleBlock aff i c) ->
        srcOrExample bk "org:element:example-block" aff "" i c
          `bindingAff` aff
          `bindingText` do
            "content" ## pure $ T.intercalate "\n" (srcLineContent <$> c)
      (SrcBlock aff lang i _ c) ->
        srcOrExample bk "org:element:src-block" aff lang i c
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
          `binding` switchCases @elm "keyword:textual"
      Keyword k (ParsedKeyword _ v) ->
        call "org:element:keyword"
          `bindingText` do
            "keyword:key" ## pure k
          `binding` switchCases @elm "keyword:parsed"
          `binding` do
            "keyword:value" ## const $ expandOrgObjects bk v
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
        `binding` parsedKwExpansions bk aff "akw:"
        `bindingText` textKwExpansions aff "akw:"
    expEls :: [OrgElement] -> Expansion tag elm
    expEls o = const $ expandOrgElements bk o
    call x = callExpansion x (pure nullEl)

expandOrgSections ::
  forall tag obj elm.
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  [OrgSection] ->
  Ondim tag [elm]
expandOrgSections _ [] = pure []
expandOrgSections bk@(ExportBackend {..}) sections@(fstSection : _) = do
  let level = sectionLevel fstSection
  hlevels <- getSetting orgExportHeadlineLevels
  shift <- getSetting headlineLevelShift
  callExpansion "org:sections" (pure $ nullEl)
    `binding` do
      if level + shift > hlevels
        then switchCases @elm "over-level"
        else switchCases @elm "normal"
      "sections" ## \x ->
        mergeLists $
          join <$> forM sections \section@(OrgSection {..}) ->
            children x
              `binding` do
                "section:headline"
                  ## const
                  $ toList <$> expandOrgObjects bk sectionTitle
                "tags" ## \inner ->
                  join <$> forM sectionTags (`tags` inner)
              `binding` do
                "section:children" ## const $ toList <$> expandOrgElements bk sectionChildren
                "section:subsections" ## const $
                  withoutText "priority" $
                    withoutText "todo-state" $
                      withoutText "todo-name" $
                        expandOrgSections bk sectionSubsections
                "h-n" ## hN (sectionLevel + shift)
              `bindingText` do
                for_ sectionTodo todo
                for_ sectionPriority priority
                for_ (toPairs $ sectionProperties) \(k, v) ->
                  "prop:" <> k ## pure v
                "anchor" ## pure $ sectionAnchor
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
  forall tag obj elm doc.
  OndimNode tag doc =>
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  OrgDocument ->
  doc ->
  Ondim tag doc
liftDocument bk (OrgDocument {..}) node = do
  modify (\s -> s {allFootnotes = documentFootnotes})
  liftSubstructures node
    `binding` do
      parsedKwExpansions
        bk
        (keywordsFromList $ documentKeywords)
        "doc:kw:"
    `bindingText` do
      textKwExpansions
        (keywordsFromList $ documentKeywords)
        "doc:kw:"
      for_ (toPairs $ documentProperties) \(k, v) ->
        "doc:prop:" <> k ## pure v
    `binding` do
      "doc:children" ## const $ expandOrgElements bk documentChildren
      "doc:sections" ## const $ expandOrgSections bk documentSections
      "doc:footnotes" ## \node' -> do
        fns <-
          gets (toPairs . snd . footnoteCounter)
            <&> mapMaybe \(ref, num) ->
              (,num) <$> lookup ref documentFootnotes
        if not (null fns)
          then
            children node'
              `binding` do
                "footnote-defs" ## \inner ->
                  join <$> forM fns \(els, num) ->
                    children @elm inner
                      `bindingText` do
                        "footnote-def:number" ## pure (show num)
                      `binding` do
                        "footnote-def:content" ## const $ expandOrgElements bk els
          else pure []

plainList ::
  forall tag obj elm.
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  ListType ->
  [ListItem] ->
  Ondim tag [elm]
plainList bk@(ExportBackend {..}) kind items =
  callExpansion "org:element:plain-list" (pure $ nullEl)
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
    listItems :: Expansion tag elm
    listItems inner =
      mergeLists $
        join <$> forM items \(ListItem _ i cbox t c) ->
          children inner
            `bindingText` do
              "counter-set" ## pure $ maybe "" show i
              "checkbox" ## pure $ maybe "" checkbox cbox
            `binding` do
              "descriptive-tag" ## const $ expandOrgObjects bk t
            `binding` do
              "list-item-content" ## const $ doPlainOrPara c
      where
        doPlainOrPara :: [OrgElement] -> Ondim tag [elm]
        doPlainOrPara [Paragraph _ objs] = plainObjsToEls <$> (expandOrgObjects bk objs)
        doPlainOrPara els = expandOrgElements bk els

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
  forall tag obj elm.
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  Text ->
  Affiliated ->
  Text ->
  Maybe Int ->
  [SrcLine] ->
  Ondim tag [elm]
srcOrExample (ExportBackend {..}) name aff lang stNumber lins =
  callExpansion name (pure $ nullEl)
    `binding` ("src-lines" ## runLines)
    `bindingText` do
      whenJust stNumber \num ->
        "starting-number" ## pure $ show num
      "content" ## pure $ T.intercalate "\n" (srcLineContent <$> lins)
  where
    runLines :: Expansion tag obj
    runLines inner = do
      cP <- liftO contentPretty
      intercalate (plain "\n")
        <$> mapM (flip lineExps inner) (zip3 [0 ..] lins cP)

    number offset =
      whenJust ((offset +) <$> stNumber) \n ->
        "number" ## pure $ show n

    contentPretty =
      let code = T.intercalate "\n" (srcLineContent <$> lins)
      in sequence <$> srcPretty aff lang code

    bPretty p = whenJust p \inls -> "content-pretty" ## const $ pure inls

    lineExps (offset, SrcLine c, pretty) inner =
      switch "plain" inner
        `bindingText` do
          number offset
          "content" ## pure c
        `binding` bPretty pretty
    lineExps (offset, RefLine i ref c, pretty) inner =
      switch "ref" inner
        `bindingText` do
          number offset
          "ref" ## pure ref
          "id" ## pure i
          "content" ## pure c
        `binding` bPretty pretty

timestamp ::
  forall tag obj elm.
  BackendC tag obj elm =>
  ExportBackend tag obj elm ->
  TimestampData ->
  Ondim tag [obj]
timestamp (ExportBackend {..}) ts =
  callExpansion "org:object:timestamp" (pure $ nullObj)
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
          switchCases @obj (active a <> "-range")
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

    tsDate :: Day -> Expansion tag obj
    tsDate day input' = do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ stringify input
      pure . plain . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> Expansion tag obj
    tsTime time input' = do
      input <- input'
      locale <- getSetting timeLocale
      let format = toString $ stringify input
      maybe (pure []) (pure . plain . toText . formatTime locale format) time
