{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Org.Exporters.Common
  ( module Org.Exporters.Common
  , module Ondim
  )
where

import Data.Aeson (Result (..), Value (..), decode, fromJSON, toJSON)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (Day, TimeOfDay (..), defaultTimeLocale, formatTime, fromGregorian)
import Ondim hiding
  ( Expansion
  , ExpansionMap
  , Filter
  , FilterMap
  , GlobalExpansion
  , GlobalFilter
  , Ondim
  , OndimState
  , SomeExpansion
  )
import Ondim qualified
import Ondim.Extra
import Org.Data.Entities qualified as Data
import Org.Exporters.Processing (resolveLinks, runPipeline)
import Org.Exporters.Processing.OrgData
import Org.Exporters.Processing.SpecialStrings (doSpecialStrings)
import Org.Parser (evalOrgMaybe)
import Org.Parser.Elements (elements)
import Org.Parser.Objects (plainMarkupContext, standardSet)
import Org.Types
import Paths_org_exporters (getDataDir)
import System.FilePath (isRelative, takeExtension, (-<.>), (</>))

type ExporterMonad m = ReaderT OrgData m

type Ondim m a = Ondim.Ondim (ExporterMonad m) a
type OndimState m = Ondim.OndimState (ExporterMonad m)

type Expansion m a = Ondim.Expansion (ExporterMonad m) a
type SomeExpansion m = Ondim.SomeExpansion (ExporterMonad m)
type GlobalExpansion m = Ondim.GlobalExpansion (ExporterMonad m)
type ExpansionMap m = Ondim.ExpansionMap (ExporterMonad m)

type Filter m a = Ondim.Filter (ExporterMonad m) a
type GlobalFilter m = Ondim.GlobalFilter (ExporterMonad m)
type FilterMap m = Ondim.FilterMap (ExporterMonad m)

data ExportBackend m obj elm = ExportBackend
  { nullObj :: obj
  , nullEl :: elm
  , plain :: Text -> [obj]
  , softbreak :: [obj]
  , exportSnippet :: Text -> Text -> [obj]
  , macro :: Text -> [Text] -> Ondim m [obj]
  , inlBabelCall :: BabelCall -> Ondim m [obj]
  , srcPretty :: Keywords -> Text -> Text -> Ondim m (Maybe [[obj]])
  , affiliatedMap :: Keywords -> ExpansionMap m
  , rawBlock :: Text -> Text -> [elm]
  , plainObjsToEls :: [obj] -> [elm]
  , stringify :: obj -> Text
  , customElement :: OrgElement -> Maybe (Ondim m [elm])
  , customObject :: OrgObject -> Maybe (Ondim m [obj])
  }

templateDir :: IO FilePath
templateDir = (</> "templates") <$> getDataDir

getSetting :: Monad m => (ExporterSettings -> b) -> Ondim m b
getSetting f = asks (f . exporterSettings)

withSettings :: Monad m => ExporterSettings -> Ondim m a -> Ondim m a
withSettings x = local (\s -> s {exporterSettings = x})

withSettingsExp :: (Monad m, OndimNode a) => Expansion m a
withSettingsExp node = do
  attrs <-
    KM.fromList . map (first K.fromText)
      <$> attributes node
  x <- toJSON <$> getSetting id
  let merged (Object x') =
        Object $ (`KM.mapWithKey` x') \k v ->
          fromMaybe v $
            decode . encodeUtf8 =<< KM.lookup k attrs
      merged y = y
  case do fromJSON (merged x) of
    Success s -> withSettings s $ liftChildren node
    Error e -> throwTemplateError (toText e)

justOrIgnore :: Monad m => Maybe a -> (a -> Expansion m b) -> Expansion m b
justOrIgnore = flip (maybe ignore)

keywordsMap ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  Keywords ->
  ExpansionMap m
keywordsMap bk = mapExp (kwValueExp bk)

kwValueExp ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  KeywordValue ->
  SomeExpansion m
kwValueExp bk = \case
  ParsedKeyword t -> someExpansion $ const $ expandOrgObjects bk t
  ValueKeyword t -> textData t
  BackendKeyword t -> namespace $ assocsExp textData t

-- | Text expansion for link target.
linkTarget ::
  Monad m =>
  LinkTarget ->
  ExpansionMap m
linkTarget tgt = do
  "target" #@ case tgt of
    URILink "file" (changeExtension -> file)
      | isRelative file -> toText file
      | otherwise -> "file:///" <> T.dropWhile (== '/') (toText file)
    URILink scheme uri -> scheme <> ":" <> uri
    InternalLink anchor -> "#" <> anchor
    UnresolvedLink other -> other
  case tgt of
    URILink scheme uri -> do
      "scheme" #@ scheme
      "path" #@ uri
      "extension" #@ uri
    InternalLink {} -> "scheme" #@ "internal"
    _ -> pure ()
  where
    changeExtension (toString -> file) =
      if takeExtension file == ".org"
        then file -<.> ".html"
        else file

type BackendC m obj elm =
  ( GlobalConstraints m obj
  , GlobalConstraints m elm
  )

parseObjectsExp ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  Expansion m obj
parseObjectsExp bk self = do
  opts <- asks parserOptions
  txt <- fromMaybe "" <$> lookupAttr "text" self
  case evalOrgMaybe opts parser txt of
    Just parsed ->
      let (solved, _) =
            runPipeline . getCompose $
              traverse resolveLinks $
                toList parsed
       in expandOrgObjects bk solved
    Nothing -> throwTemplateError $ "Could not parse " <> show txt
  where
    parser = plainMarkupContext standardSet

parseElementsExp ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  Expansion m elm
parseElementsExp bk self = do
  opts <- asks parserOptions
  txt <- fromMaybe "" <$> lookupAttr "text" self
  case evalOrgMaybe opts elements txt of
    Just parsed ->
      let (solved, _) =
            runPipeline . getCompose $
              traverse resolveLinks $
                toList parsed
       in expandOrgElements bk solved
    Nothing -> throwTemplateError $ "Could not parse " <> show txt

expandOrgObjects ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  [OrgObject] ->
  Ondim m [obj]
expandOrgObjects = foldMapM . expandOrgObject

expandOrgObject ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  OrgObject ->
  Ondim m [obj]
expandOrgObject bk@(ExportBackend {..}) obj = do
  s <- getSetting orgInlineImageRules
  (`fromMaybe` customObject obj)
    case obj of
      (Plain txt) -> do
        specialStrings <- getSetting orgExportWithSpecialStrings
        pure $
          plain (if specialStrings then doSpecialStrings txt else txt)
      SoftBreak ->
        pure softbreak
      LineBreak ->
        callExpansion "org:object:linebreak" nullObj
      (Code txt) ->
        "org:object:code" `callWith` do
          "content" #@ txt
      (Entity name) -> do
        withEntities <- getSetting orgExportWithEntities
        case Map.lookup name Data.defaultEntitiesMap of
          Just (Data.Entity _ latex mathP html ascii latin utf8)
            | withEntities ->
                "org:object:entity" `callWith` do
                  "if-math" ## ifElse @obj mathP
                  "latex" #@ latex
                  "ascii" #@ ascii
                  "html" #@ html
                  "latin" #@ latin
                  "utf8" #@ utf8
          _ -> pure $ plain ("\\" <> name)
      (LaTeXFragment ftype txt) ->
        "org:object:latex-fragment" `callWith` do
          "content" #@ txt
          "type" #@ case ftype of
            InlMathFragment -> "inline"
            DispMathFragment -> "display"
            RawFragment -> "raw"
      (ExportSnippet backend code) ->
        pure $ exportSnippet backend code
      (Src lang _params txt) ->
        "org:object:src" `callWith` do
          "language" #@ lang
          "content" #@ txt
      (Target anchor name) ->
        "org:object:target" `callWith` do
          "anchor" #@ anchor
          "name" #@ name
      (Italic objs) ->
        "org:object:italic" `callWith` do
          "content" ## expObjs objs
      (Underline objs) ->
        "org:object:underline" `callWith` do
          "content" ## expObjs objs
      (Bold objs) ->
        "org:object:bold" `callWith` do
          "content" ## expObjs objs
      (Strikethrough objs) ->
        "org:object:strikethrough" `callWith` do
          "content" ## expObjs objs
      (Superscript objs) ->
        "org:object:superscript" `callWith` do
          "content" ## expObjs objs
      (Subscript objs) ->
        "org:object:subscript" `callWith` do
          "content" ## expObjs objs
      (Quoted qtype objs) ->
        "org:object:quoted" `callWith` do
          "content" ## expObjs objs
          "type" #@ case qtype of
            SingleQuote -> "single"
            DoubleQuote -> "double"
      (Verbatim txt) ->
        "org:object:verbatim" `callWith` do
          "content" #@ txt
      (Link tgt [])
        | isImgTarget s tgt -> do
            "org:object:image" `callWith` do
              linkTarget tgt
        | otherwise -> expandOrgObject bk (Link tgt [Plain $ linkTargetToText tgt])
      (Link tgt objs) ->
        "org:object:link" `callWith` do
          linkTarget tgt
          "content" ## expObjs objs
      (Timestamp ts) ->
        "org:object:timestamp" `callWith` do
          timestamp bk ts
      (FootnoteRef (FootnoteRefLabel name)) -> do
        def <- asks ((Map.!? name) . footnotes)
        "org:object:footnote-ref" `callWith` do
          whenJust def \thing ->
            whenLeft () thing \objs ->
              "content" ## const $ expandOrgObjects bk objs
          "key" #@ name
      (FootnoteRef _) -> pure []
      (Cite _) ->
        pure $ plain "(unresolved citation)" -- TODO
      (StatisticCookie c) ->
        "org:object:statistic-cookie" `callWith` do
          case c of
            Left (show -> n, show -> d) -> do
              "type" #@ "fraction"
              "numerator" #@ n
              "denominator" #@ d
              "value" #@ n <> "/" <> d
            Right (show -> p) -> do
              "type" #@ "percentage"
              "percentage" #@ p
              "value" #@ p <> "%"
      Macro key args -> macro key args
      InlBabelCall args -> inlBabelCall args
  where
    expObjs :: [OrgObject] -> Expansion m obj
    expObjs o = const $ expandOrgObjects bk o
    callWith x y =
      callExpansion x nullObj
        `binding` do "this" #. y

expandOrgElements ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  [OrgElement] ->
  Ondim m [elm]
expandOrgElements = foldMapM . expandOrgElement

expandOrgElement ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  OrgElement ->
  Ondim m [elm]
expandOrgElement bk@(ExportBackend {..}) el = do
  s <- getSetting orgInlineImageRules
  (`fromMaybe` customElement el)
    case el of
      (Paragraph aff [Link tgt []])
        | isImgTarget s tgt ->
            "org:element:figure" `callWith` do
              affMap aff
              linkTarget tgt
      (Paragraph aff c) ->
        "org:element:paragraph" `callWith` do
          affMap aff
          "content" ## const $ expandOrgObjects bk c
      (GreaterBlock aff Quote c) ->
        "org:element:quote-block" `callWith` do
          affMap aff
          "content" ## expEls c
      (GreaterBlock aff Center c) ->
        "org:element:center-block" `callWith` do
          affMap aff
          "content" ## expEls c
      (GreaterBlock aff (Special cls) c) ->
        "org:element:special-block" `callWith` do
          affMap aff
          "name" #@ cls
          "content" ## expEls c
      (PlainList aff k i) ->
        "org:element:plain-list" `callWith` do
          affMap aff
          plainList bk k i
      (Drawer name els) ->
        "org:element:drawer" `callWith` do
          "content" ## expEls els
          "name" #@ name
      (ExportBlock lang code) ->
        pure $ rawBlock lang code
      (ExampleBlock aff switches c) ->
        "org:element:example-block" `callWith` do
          affMap aff
          srcOrExample bk aff "" c
          "content" #@ srcLinesToText c
      (SrcBlock aff lang switches _ c) ->
        "org:element:src-block" `callWith` do
          affMap aff
          srcOrExample bk aff lang c
          "language" #@ lang
          "content" #@ srcLinesToText c
      (LaTeXEnvironment aff env text) ->
        "org:element:latex-environment" `callWith` do
          affMap aff
          "name" #@ env
          "content" #@ text
      (Table aff rs) ->
        "org:element:table" `callWith` do
          affMap aff
          table bk rs
      HorizontalRule ->
        callExpansion "org:element:horizontal-rule" nullEl
      Keyword k v ->
        "org:element:keyword" `callWith` do
          "key" #@ k
          case v of
            ValueKeyword v' -> do
              "value" #@ v'
            ParsedKeyword v' -> do
              "value" ## const $ expandOrgObjects bk v'
            _ -> pure ()
      FootnoteDef {} -> pure []
      VerseBlock {} -> error "TODO"
  where
    affMap aff = do
      affiliatedMap aff
      "akw" #. keywordsMap bk aff
    expEls :: [OrgElement] -> Expansion m elm
    expEls o = const $ expandOrgElements bk o
    callWith x y =
      callExpansion x nullEl
        `binding` do "this" #. y

sectionExp ::
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  OrgSection ->
  ExpansionMap m
sectionExp bk section@(OrgSection {..}) = do
  "title" ## const $ expandOrgObjects bk sectionTitle
  "tags" #. listExp textData sectionTags
  "children" ## const $ expandOrgElements bk sectionChildren
  "subsections" ## expandOrgSections bk sectionSubsections
  planning sectionPlanning
  for_ sectionTodo todo
  for_ sectionPriority priority
  "prop" #. mapExp textData sectionProperties
  "anchor" #@ sectionAnchor
  -- Debug
  "debug.ast" #@ show section
  where
    todo (TodoKeyword st nm) =
      "todo" #. do
        "state" #@ todost st
        "name" #@ nm
    todost Done = "done"
    todost Todo = "todo"
    priority p =
      "priority" #@ case p of
        (LetterPriority c) -> T.singleton c
        (NumericPriority n) -> show n
    planning (PlanningInfo {..}) =
      let assocs =
            catMaybes
              [ ("closed",) <$> planningClosed
              , ("deadline",) <$> planningDeadline
              , ("scheduled",) <$> planningScheduled
              ]
          renderTs a = someExpansion $ const $ expandOrgObject bk (Timestamp a)
       in "planning" #. assocsExp renderTs assocs

expandOrgSections ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  [OrgSection] ->
  Expansion m elm
expandOrgSections bk sections inner = do
  hlevels <- getSetting orgExportHeadlineLevels
  shift <- getSetting headlineLevelShift
  join <$> forM (groupByLevel sections) \(level, sameLvlSecs) ->
    callExpansion "org:sections" inner
      `binding` do
        "this" #. do
          "type" #@ if level + shift > hlevels then "over-level" else "normal"
          "level" #@ show $ level + shift
          listExp (namespace . sectionExp bk) sameLvlSecs
  where
    groupByLevel :: [OrgSection] -> [(Int, [OrgSection])]
    groupByLevel = foldr go []
      where
        go s [] = [(sectionLevel s, [s])]
        go s l@((i, ss) : ls)
          | sectionLevel s == i = (i, s : ss) : ls
          | otherwise = (sectionLevel s, [s]) : l

liftDocument ::
  forall m obj elm doc.
  OndimNode doc =>
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  OrgData ->
  OrgDocument ->
  doc ->
  Ondim m doc
liftDocument bk datum doc node =
  bindDocument bk datum doc (liftSubstructures node)

bindDocument ::
  forall m obj elm doc.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  -- | Prefix for expansion names
  OrgData ->
  OrgDocument ->
  Ondim m doc ->
  Ondim m doc
bindDocument bk datum (OrgDocument {..}) node = do
  local (const datum) node
    `binding` do
      "doc" #. do
        "kw" #. keywordsMap bk (keywords datum)
        "prop" #. mapExp textData documentProperties
        "children" ## const $ expandOrgElements bk documentChildren
        "sections" ## expandOrgSections bk documentSections
        "footnotes" #. mapExp fnExp (footnotes datum)
        "tags" #. listExp textData (filetags datum)
  where
    fnExp =
      someExpansion
        . const
        . expandOrgElements bk
        . either (one . Paragraph mempty) id

table ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  [TableRow] ->
  ExpansionMap m
table bk rows = do
  "table:head" #* \inner ->
    case tableHead of
      Just rs ->
        liftChildren inner `binding` do
          "head:rows" #* tableRows rs
      Nothing -> pure []
  "table:bodies" ## tableBodies
  where
    (groups, props) = foldr go ([], []) rows
      where
        go (ColumnPropsRow p) ~(l, r) = (l, p : r)
        go (StandardRow cs) ~(l, r)
          | g : gs <- l = ((cs : g) : gs, r)
          | [] <- l = ([cs] : l, r)
        go RuleRow ~(l, r) = ([] : l, r)

    (tableHead, bodies) = case groups of
      [] -> (Nothing, [])
      [b] -> (Nothing, [b])
      h : b -> (Just h, b)

    tableBodies :: Expansion m elm
    tableBodies inner =
      join <$> forM bodies \body ->
        liftChildren inner `binding` do
          "body:rows" #* tableRows body

    tableRows :: [[TableCell]] -> GlobalExpansion m
    tableRows rs inner =
      join <$> forM rs \cells ->
        liftChildren inner
          `binding` do
            "row:cells" ## \inner' ->
              join <$> forM (zip cells alignment) \(row, alig) ->
                liftChildren @obj inner'
                  `binding` do
                    "cell:content" ## const $ expandOrgObjects bk row
                    for_ alig \a ->
                      "cell:alignment" #@ case a of
                        AlignLeft -> "left"
                        AlignRight -> "right"
                        AlignCenter -> "center"

    alignment =
      (++ repeat Nothing) $
        fromMaybe [] $
          listToMaybe
            props

plainList ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  ListType ->
  [ListItem] ->
  ExpansionMap m
plainList bk@(ExportBackend {..}) kind items = do
  "items" #. listExp (namespace . listItemExp) items
  case kind of
    Ordered OrderedNum -> "type" #@ "ordered-num"
    Ordered OrderedAlpha -> "type" #@ "ordered-alpha"
    Descriptive -> "type" #@ "descriptive"
    Unordered b -> do
      "type" #@ "unordered"
      "bullet" #@ one b
  where
    listItemExp :: ListItem -> ExpansionMap m
    listItemExp (ListItem _ i cbox t c) = do
      for_ i \i' -> "counter-set" #@ show i'
      for_ cbox \cbox' -> "checkbox" #@ checkbox cbox'
      unless (null t) $
        "descriptive-tag" ## const $
          expandOrgObjects bk t
      "content" ## const $ doPlainOrPara c
      where
        doPlainOrPara :: [OrgElement] -> Ondim m [elm]
        doPlainOrPara [Paragraph _ objs] = plainObjsToEls <$> expandOrgObjects bk objs
        doPlainOrPara els = expandOrgElements bk els

        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox = "partial"

srcOrExample ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  Keywords ->
  Text ->
  [SrcLine] ->
  ExpansionMap m
srcOrExample (ExportBackend {..}) aff lang lins = do
  "src-lines" ## runLines
  "content" #@ srcLinesToText lins
  where
    runLines :: Expansion m obj
    runLines inner = do
      cP <- contentPretty
      intercalate (plain "\n")
        <$> mapM (`lineExps` inner) (zip lins cP)

    contentPretty =
      (++ repeat Nothing) . sequence <$> srcPretty aff lang (srcLinesToText lins)

    bPretty p = whenJust p \inls -> "content-pretty" ## const $ pure inls

    lineExps (SrcLine c, pretty) inner =
      switch "plain" inner
        `binding` do
          "content" #@ c
          bPretty pretty
    lineExps (RefLine i ref c, pretty) inner =
      switch "ref" inner
        `binding` do
          "ref" #@ ref
          "id" #@ i
          "content" #@ c
          bPretty pretty

timestamp ::
  forall m obj elm.
  BackendC m obj elm =>
  ExportBackend m obj elm ->
  TimestampData ->
  ExpansionMap m
timestamp (ExportBackend {..}) ts =
  case ts of
    TimestampData a (dateToDay -> d, fmap toTime -> t, r, w) -> do
      dtExps d t r w
      switchCases (active a <> "-single")
    TimestampRange
      a
      (dateToDay -> d1, fmap toTime -> t1, r1, w1)
      (dateToDay -> d2, fmap toTime -> t2, r2, w2) -> do
        "from" #* \x -> liftChildren x `binding` dtExps d1 t1 r1 w1
        "to" #* \x -> liftChildren x `binding` dtExps d2 t2 r2 w2
        switchCases (active a <> "-range")
  where
    dtExps d t r w = do
      "repeater"
        #* justOrIgnore r \r' x -> liftChildren x `binding` tsMark r'
      "warning-period"
        #* justOrIgnore w \w' x -> liftChildren x `binding` tsMark w'
      "ts-date" ## tsDate d
      "ts-time" ## tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> ExpansionMap m
    tsMark (_, v, c) = do
      "value" #@ show v
      "unit" #@ one c

    dateToDay (y, m, d, _) = fromGregorian (toInteger y) m d
    toTime (h, m) = TimeOfDay h m 0

    tsDate :: Day -> Expansion m obj
    tsDate day input = do
      let format = toString $ stringify input
          locale = defaultTimeLocale -- TODO
      pure . plain . toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> Expansion m obj
    tsTime time input = do
      let format = toString $ stringify input
          locale = defaultTimeLocale -- TODO
      maybe (pure []) (pure . plain . toText . formatTime locale format) time
