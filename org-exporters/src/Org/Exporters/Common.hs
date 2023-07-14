{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Org.Exporters.Common
  ( ExportBackend (..)
  , templateDir
  , objectExp
  , objectsExp
  , elementExp
  , elementsExp
  , documentExp
  , parserExpObjs
  , parserExpElms
  , module Ondim
  )
where

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (Day, TimeOfDay (..), defaultTimeLocale, formatTime, fromGregorian)
import Ondim
import Ondim.Extra
import Org.Data.Entities qualified as Data
import Org.Exporters.Processing (resolveLinks, runPipeline)
import Org.Exporters.Processing.OrgData
import Org.Exporters.Processing.SpecialStrings (doSpecialStrings)
import Org.Parser (evalOrgMaybe)
import Org.Parser.Definitions (OrgOptions, OrgParser)
import Org.Parser.Elements (elements)
import Org.Parser.Objects (plainMarkupContext, standardSet)
import Org.Types
import Org.Walk (MWTag, MultiWalk)
import Paths_org_exporters (getDataDir)
import System.FilePath (isRelative, takeExtension, (-<.>), (</>))

data ExportBackend m = ExportBackend
  { affiliatedMap :: Keywords -> ExpansionMap m
  , macro :: Text -> [Text] -> SomeExpansion m
  , babelCall :: BabelCall -> SomeExpansion m
  , srcPretty :: Keywords -> Text -> Text -> SomeExpansion m
  , customElement :: ExportBackend m -> OrgData -> OrgElement -> Maybe (ExpansionMap m)
  , customObject :: ExportBackend m -> OrgData -> OrgObject -> Maybe (ExpansionMap m)
  }

templateDir :: IO FilePath
templateDir = (</> "templates") <$> getDataDir

keywordsMap ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  Keywords ->
  ExpansionMap m
keywordsMap bk odata = mapExp (kwValueExp bk odata)

kwValueExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  KeywordValue ->
  SomeExpansion m
kwValueExp bk odata = \case
  ParsedKeyword t -> namespace $ objectsExp bk odata t
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

parserExp ::
  (Monad m, MultiWalk MWTag a) =>
  OrgOptions ->
  OrgParser [a] ->
  ([a] -> ExpansionMap m) ->
  GlobalExpansion m
parserExp opts parser expand self = do
  txt <- fromMaybe "" <$> lookupAttr "text" self
  case evalOrgMaybe opts parser txt of
    Just parsed ->
      let (solved, _) =
            runPipeline . getCompose $
              traverse resolveLinks parsed
       in liftChildren self
            `binding` expand solved
    Nothing -> throwTemplateError $ "Could not parse " <> show txt

parserExpObjs ::
  ExportBackend m ->
  OrgData ->
  GlobalExpansion m
parserExpObjs bk odata =
  parserExp
    (parserOptions odata)
    (toList <$> plainMarkupContext standardSet)
    (objectsExp bk odata)

parserExpElms ::
  ExportBackend m ->
  OrgData ->
  GlobalExpansion m
parserExpElms bk odata = do
  parserExp
    (parserOptions odata)
    (toList <$> elements)
    (elementsExp bk odata)

objectsExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  [OrgObject] ->
  ExpansionMap m
objectsExp bk odata = listExp (namespace . objectExp bk odata)

objectExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgObject ->
  ExpansionMap m
objectExp bk@ExportBackend {..} odata@OrgData {..} obj =
  (`fromMaybe` customObject bk odata obj)
    case obj of
      (Plain txt) -> do
        tag #@ "plain"
        let specialStrings = orgExportWithSpecialStrings exporterSettings
        content #@ if specialStrings then doSpecialStrings txt else txt
      LineBreak -> do
        tag #@ "linebreak"
      (Code txt) -> do
        tag #@ "code"
        content #@ txt
      (Entity name)
        | orgExportWithEntities exporterSettings -> do
            tag #@ "entity"
            let Data.Entity {..} = Data.defaultEntitiesMap Map.! name
            "if-math" #* ifElse latexMathP
            "latex" #@ latexReplacement
            "ascii" #@ asciiReplacement
            "html" #@ htmlReplacement
            "latin" #@ latin1Replacement
            "utf8" #@ utf8Replacement
        | otherwise -> objectExp bk odata (Plain $ "\\" <> name)
      (LaTeXFragment ftype txt) -> do
        tag #@ "latex-fragment"
        content #@ txt
        "type" #@ case ftype of
          InlMathFragment -> "inline"
          DispMathFragment -> "display"
          RawFragment -> "raw"
      (ExportSnippet backend code) -> do
        tag #@ "export-snippet"
        "backend" #@ backend
        content #@ code
      (Src lang _params txt) -> do
        tag #@ "src"
        "language" #@ lang
        content #@ txt
      (Target anchor name) -> do
        tag #@ "target"
        "anchor" #@ anchor
        "name" #@ name
      (Italic objs) -> do
        tag #@ "italic"
        content #. expObjs objs
      (Underline objs) -> do
        tag #@ "underline"
        content #. expObjs objs
      (Bold objs) -> do
        tag #@ "bold"
        content #. expObjs objs
      (Strikethrough objs) -> do
        tag #@ "strikethrough"
        content #. expObjs objs
      (Superscript objs) -> do
        tag #@ "superscript"
        content #. expObjs objs
      (Subscript objs) -> do
        tag #@ "subscript"
        content #. expObjs objs
      (Quoted qtype objs) -> do
        tag #@ "quoted"
        content #. expObjs objs
        "type" #@ case qtype of
          SingleQuote -> "single"
          DoubleQuote -> "double"
      (Verbatim txt) -> do
        tag #@ "verbatim"
        content #@ txt
      (Link tgt [])
        | isImgTarget (orgInlineImageRules exporterSettings) tgt -> do
            tag #@ "image"
            linkTarget tgt
        | otherwise -> objectExp bk odata (Link tgt [Plain $ linkTargetToText tgt])
      (Link tgt objs) -> do
        tag #@ "link"
        linkTarget tgt
        content #. expObjs objs
      (Timestamp ts) -> do
        tag #@ "timestamp"
        timestamp ts
      (FootnoteRef (FootnoteRefLabel name)) -> do
        tag #@ "footnote-ref"
        let def = footnotes Map.!? name
        whenJust def \thing ->
          whenLeft () thing \objs ->
            content #. expObjs objs
        "key" #@ name
      (FootnoteRef _) -> pass
      (Cite _) -> pass -- TODO
      (StatisticCookie c) -> do
        tag #@ "statistic-cookie"
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
      Macro name args -> do
        tag #@ "macro"
        "name" #@ name
        "arguments" #. listExp textData args
        content #: macro name args
      InlBabelCall args -> do
        tag #@ "babel-call"
        content #: babelCall args
  where
    tag = "tag"
    content = "content"
    expObjs = objectsExp bk odata

elementsExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  [OrgElement] ->
  ExpansionMap m
elementsExp bk odata = listExp (namespace . elementExp bk odata)

elementDataExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgElementData ->
  ExpansionMap m
elementDataExp bk odata@OrgData {..} = \case
  (Paragraph [Link tgt []])
    | isImgTarget (orgInlineImageRules exporterSettings) tgt -> do
        tag #@ "figure"
        linkTarget tgt
  (Paragraph c) -> do
    tag #@ "paragraph"
    content #. objectsExp bk odata c
  (GreaterBlock Quote c) -> do
    tag #@ "quote-block"
    content #. expEls c
  (GreaterBlock Center c) -> do
    tag #@ "center-block"
    content #. expEls c
  (GreaterBlock (Special cls) c) -> do
    tag #@ "special-block"
    "name" #@ cls
    content #. expEls c
  (PlainList k i) -> do
    tag #@ "plain-list"
    plainList bk odata k i
  (Drawer name els) -> do
    tag #@ "drawer"
    content #. expEls els
    "name" #@ name
  (ExportBlock backend code) -> do
    tag #@ "export-block"
    "backend" #@ backend
    content #@ code
  (ExampleBlock _ c) -> do
    tag #@ "example-block"
    srcOrExample c
  (SrcBlock lang _ attrs c) -> do
    tag #@ "src-block"
    srcOrExample c
    "language" #@ lang
    "attributes" #. assocsExp textData attrs
  (LaTeXEnvironment env text) -> do
    tag #@ "latex-environment"
    "name" #@ env
    content #@ text
  (Table rs) -> do
    tag #@ "table"
    table bk odata rs
  HorizontalRule ->
    tag #@ "horizontal-rule"
  Keyword k v -> do
    tag #@ "keyword"
    "key" #@ k
    case v of
      ValueKeyword v' -> do
        "value" #@ v'
      ParsedKeyword v' -> do
        "value" #. objectsExp bk odata v'
      _ -> pass
  FootnoteDef {} -> pass
  Comment {} -> pass
  VerseBlock {} -> pass -- TODO
  where
    tag = "tag"
    content = "content"
    expEls = elementsExp bk odata

elementExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgElement ->
  ExpansionMap m
elementExp bk@ExportBackend {..} odata el@(OrgElement aff eldata) = do
  (`fromMaybe` customElement bk odata el) do
    elementDataExp bk odata eldata
    affiliatedMap aff
    "akw" #. keywordsMap bk odata aff

sectionExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgSection ->
  ExpansionMap m
sectionExp bk odata (OrgSection {..}) = do
  "title" #. objectsExp bk odata sectionTitle
  "tags" #. listExp textData sectionTags
  "children" #. elementsExp bk odata sectionChildren
  "subsections" #. sectionsExp bk odata sectionSubsections
  planning sectionPlanning
  for_ sectionTodo todo
  for_ sectionPriority priority
  "prop" #. mapExp textData sectionProperties
  "anchor" #@ sectionAnchor
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
          renderTs a = namespace $ objectExp bk odata (Timestamp a)
       in "planning" #. assocsExp renderTs assocs

sectionsExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  [OrgSection] ->
  ExpansionMap m
sectionsExp bk odata sections = listExp (namespace . sameLevel) (groupByLevel sections)
  where
    hlevels = orgExportHeadlineLevels (exporterSettings odata)
    shift = headlineLevelShift (exporterSettings odata)
    sameLevel (level, sameLvlSecs) = do
      "type" #@ if level + shift > hlevels then "over-level" else "normal"
      "level" #@ show $ level + shift
      listExp (namespace . sectionExp bk odata) sameLvlSecs

    groupByLevel :: [OrgSection] -> [(Int, [OrgSection])]
    groupByLevel = foldr go []
      where
        go s [] = [(sectionLevel s, [s])]
        go s l@((i, ss) : ls)
          | sectionLevel s == i = (i, s : ss) : ls
          | otherwise = (sectionLevel s, [s]) : l

documentExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  -- | Prefix for expansion names
  OrgData ->
  OrgDocument ->
  ExpansionMap m
documentExp bk odata (OrgDocument {..}) = do
  "doc" #. do
    "kw" #. keywordsMap bk odata (keywords odata)
    "prop" #. mapExp textData documentProperties
    "children" #. elementsExp bk odata documentChildren
    "sections" #. sectionsExp bk odata documentSections
    "footnotes" #. mapExp fnExp (footnotes odata)
    "tags" #. listExp textData (filetags odata)
  where
    fnExp =
      namespace
        . elementsExp bk odata
        . either (one . OrgElement mempty . Paragraph) id

table ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  [TableRow] ->
  ExpansionMap m
table bk odata rows = do
  "head" #. tableRows tableHead
  "bodies" #. tableBodies
  where
    (groups, props) = foldr go ([], []) rows
      where
        go (ColumnPropsRow p) (l, r) = (l, p : r)
        go (StandardRow cs) (l, r)
          | g : gs <- l = ((cs : g) : gs, r)
          | [] <- l = ([cs] : l, r)
        go RuleRow (l, r) = ([] : l, r)

    (tableHead, bodies) = case groups of
      [] -> ([], [])
      [b] -> ([], [b])
      h : b -> (h, b)

    tableBodies = listExp (namespace . tableRows) bodies
    tableRows = listExp (namespace . tableRow)
    tableRow = listExp (namespace . tableCell) . zip alignment

    tableCell :: (Maybe ColumnAlignment, TableCell) -> ExpansionMap m
    tableCell (alig, cell) = do
      "content" #. objectsExp bk odata cell
      for_ alig \a ->
        "alignment" #@ case a of
          AlignLeft -> "left"
          AlignRight -> "right"
          AlignCenter -> "center"

    alignment =
      (++ repeat Nothing) $
        fromMaybe [] $
          listToMaybe props

plainList ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  ListType ->
  [ListItem] ->
  ExpansionMap m
plainList bk odata kind items = do
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
      "descriptive-tag" #. objectsExp bk odata t
      "content" #. elementsExp bk odata c
      where
        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox = "partial"

srcOrExample ::
  forall m.
  Monad m =>
  ExportBackend m ->
  Keywords ->
  Text ->
  [SrcLine] ->
  ExpansionMap m
srcOrExample (ExportBackend {..}) aff lang lins = do
  "lines" #. runLines
  "lines-pretty" #: srcPretty aff lang (srcLinesToText lins)
  "content" #@ srcLinesToText lins
  where
    runLines :: ExpansionMap m
    runLines = listExp (\x -> globalExpansion (lineExps x)) lins

    lineExps (SrcLine c) inner =
      switch "plain" inner
        `binding` do
          "content" #@ c
    lineExps (RefLine i ref c) inner =
      switch "ref" inner
        `binding` do
          "ref" #@ ref
          "id" #@ i
          "content" #@ c

timestamp ::
  forall m.
  Monad m =>
  TimestampData ->
  ExpansionMap m
timestamp ts =
  case ts of
    TimestampData a (dateToDay -> d, fmap toTime -> t, r, w) -> do
      dtExps d t r w
      "type" #@ active a
      "span" #@ "single"
    TimestampRange
      a
      (dateToDay -> d1, fmap toTime -> t1, r1, w1)
      (dateToDay -> d2, fmap toTime -> t2, r2, w2) -> do
        "from" #. dtExps d1 t1 r1 w1
        "to" #. dtExps d2 t2 r2 w2
        "type" #@ active a
        "span" #@ "range"
  where
    dtExps d t r w = do
      whenJust r \r' ->
        "repeater" #. tsMark r'
      whenJust w \w' ->
        "warning-period" #. tsMark w'
      "date" #* tsDate d
      "time" #* tsTime t

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> ExpansionMap m
    tsMark (_, v, c) = do
      "value" #@ show v
      "unit" #@ one c

    dateToDay (y, m, d, _) = fromGregorian (toInteger y) m d
    toTime (h, m) = TimeOfDay h m 0

    tsDate :: Day -> GlobalExpansion m
    tsDate day input = do
      format <- toString <$> lookupAttr' "format" input
      let locale = defaultTimeLocale -- TODO
      liftChildren input `binding` do
        "value" #@ toText $ formatTime locale format day

    tsTime :: Maybe TimeOfDay -> GlobalExpansion m
    tsTime time input = do
      format <- toString <$> lookupAttr' "format" input
      let locale = defaultTimeLocale -- TODO
      liftChildren input `binding` do
        whenJust time \t ->
          "value" #@ toText $ formatTime locale format t
