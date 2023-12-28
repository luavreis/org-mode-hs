{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Org.Exporters.Common
  ( ExportBackend (..)
  , objectExp
  , objectsExp
  , elementExp
  , elementsExp
  , documentExp
  , parserExpObjs
  , parserExpElms
  , queryExpElms
  , queryExpObjs
  , queryExpSecs
  , module Ondim
  )
where

import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (TimeOfDay (..), defaultTimeLocale, formatTime, fromGregorian)
import Ondim
import Ondim.Extra.Expansions (assocsExp, ifElse, listExp, lookupAttr', mapExp)
import Org.Data.Entities qualified as Data
import Org.Exporters.Processing (resolveLinks, runPipeline)
import Org.Exporters.Processing.OrgData
import Org.Exporters.Processing.SpecialStrings (doSpecialStrings)
import Org.Parser (OrgOptions, OrgParser, parseOrgMaybe)
import Org.Parser.Elements (elements)
import Org.Parser.Objects (plainMarkupContext, standardSet)
import System.FilePath (isRelative, takeExtension, (-<.>))

data ExportBackend m = ExportBackend
  { affiliatedMap :: Keywords -> ExpansionMap m
  , macro :: Text -> [Text] -> SomeExpansion m
  , babelCall :: BabelCall -> SomeExpansion m
  , srcPretty :: Keywords -> Text -> Text -> SomeExpansion m
  , customElement :: ExportBackend m -> OrgData -> OrgElement -> Maybe (ExpansionMap m)
  , customObject :: ExportBackend m -> OrgData -> OrgObject -> Maybe (ExpansionMap m)
  }

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
  case parseOrgMaybe opts parser txt of
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

queryExp ::
  ( Monad m
  , MultiWalk MWTag a
  , MultiWalk MWTag b
  ) =>
  ([b] -> ExpansionMap m) ->
  ([Attribute] -> b -> Maybe b) ->
  a ->
  GlobalExpansion m
queryExp g f thing self = do
  attrs <- attributes self
  let f' = f attrs
      result = query (maybeToList . f') thing
  liftChildren self
    `binding` do
      "result" #. g result

queryExpObjs ::
  ( Monad m
  , MultiWalk MWTag a
  ) =>
  ExportBackend m ->
  OrgData ->
  a ->
  GlobalExpansion m
queryExpObjs bk odata =
  queryExp (objectsExp bk odata) \attrs (o :: OrgObject) ->
    case L.lookup "type" attrs of
      Just tag
        | objectTag odata o == tag -> Just o
      _ -> Nothing

queryExpElms ::
  ( Monad m
  , MultiWalk MWTag a
  ) =>
  ExportBackend m ->
  OrgData ->
  a ->
  GlobalExpansion m
queryExpElms bk odata =
  queryExp (elementsExp bk odata) \attrs el@(OrgElement kws _) ->
    let go (x, y) = (,y) <$> T.stripPrefix "kw:" x
        kwFils = mapMaybe go attrs
        gop (x, y) p = (Just (ValueKeyword y) == Map.lookup x kws) && p
     in if foldr gop True kwFils
          then Just el
          else Nothing

queryExpSecs ::
  ( Monad m
  , MultiWalk MWTag a
  ) =>
  ExportBackend m ->
  OrgData ->
  a ->
  GlobalExpansion m
queryExpSecs bk odata =
  queryExp (sectionsExp bk odata) \attrs sec@(OrgSection {..}) ->
    let go (x, y) = (,y) <$> T.stripPrefix "prop:" x
        todoNm = maybe True ((todoName <$> sectionTodo ==) . Just) (L.lookup "todo-name" attrs)
        todoStName Todo = "todo"
        todoStName Done = "done"
        todoSt = maybe True ((todoStName . todoState <$> sectionTodo ==) . Just) (L.lookup "todo-state" attrs)
        level = maybe True (sectionLevel ==) (readMaybe . toString =<< L.lookup "level" attrs)
        kwFils = mapMaybe go attrs
        gop (x, y) p = (Just y == Map.lookup x sectionProperties) && p
     in if foldr gop True kwFils && todoNm && todoSt && level
          then Just sec
          else Nothing

objectsExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  [OrgObject] ->
  ExpansionMap m
objectsExp bk odata objs = do
  listExp (namespace . objectExp bk odata) objs
  "" #* \inner ->
    join <$> forM objs \obj ->
      callExpansion "org:objects" inner
        `binding` do
          "this" #. objectExp bk odata obj

objectTag :: OrgData -> OrgObject -> Text
objectTag OrgData {..} = \case
  Plain {} -> "plain"
  LineBreak -> "linebreak"
  Code {} -> "code"
  Entity {} -> "entity"
  LaTeXFragment {} -> "latex-fragment"
  ExportSnippet {} -> "export-snippet"
  Src {} -> "src"
  Target {} -> "target"
  Italic {} -> "italic"
  Underline {} -> "underline"
  Bold {} -> "bold"
  Strikethrough {} -> "strikethrough"
  Superscript {} -> "superscript"
  Subscript {} -> "subscript"
  Quoted {} -> "quoted"
  Verbatim {} -> "verbatim"
  Link tgt []
    | isImgTarget (orgInlineImageRules exporterSettings) tgt -> "image"
  Link {} -> "link"
  Timestamp {} -> "timestamp"
  FootnoteRef {} -> "footnote-ref"
  Cite {} -> "cite"
  StatisticCookie {} -> "statistic-cookie"
  Macro {} -> "macro"
  InlBabelCall {} -> "babel-call"

objectExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgObject ->
  ExpansionMap m
objectExp bk@ExportBackend {..} odata@OrgData {..} obj =
  (`fromMaybe` customObject bk odata obj) do
    "tag" #@ objectTag odata obj
    case obj of
      Plain txt -> do
        let specialStrings = orgExportWithSpecialStrings exporterSettings
        content #@ if specialStrings then doSpecialStrings txt else txt
      LineBreak -> pass
      Code txt -> content #@ txt
      Entity name
        | orgExportWithEntities exporterSettings -> do
            let Data.Entity {..} = Data.defaultEntitiesMap Map.! name
            "if-math" #* ifElse latexMathP
            "latex" #@ latexReplacement
            "ascii" #@ asciiReplacement
            "html" #@ htmlReplacement
            "latin" #@ latin1Replacement
            "utf8" #@ utf8Replacement
        | otherwise -> objectExp bk odata (Plain $ "\\" <> name)
      LaTeXFragment ftype txt -> do
        content #@ txt
        "type" #@ case ftype of
          InlMathFragment -> "inline"
          DispMathFragment -> "display"
          RawFragment -> "raw"
      ExportSnippet backend code -> do
        "backend" #@ backend
        content #@ code
      Src lang _params txt -> do
        "language" #@ lang
        content #@ txt
      Target anchor name -> do
        "anchor" #@ anchor
        "name" #@ name
      Italic objs -> content #. expObjs objs
      Underline objs -> content #. expObjs objs
      Bold objs -> content #. expObjs objs
      Strikethrough objs -> content #. expObjs objs
      Superscript objs -> content #. expObjs objs
      Subscript objs -> content #. expObjs objs
      Quoted qtype objs -> do
        content #. expObjs objs
        "type" #@ case qtype of
          SingleQuote -> "single"
          DoubleQuote -> "double"
      Verbatim txt -> content #@ txt
      Link tgt []
        | isImgTarget (orgInlineImageRules exporterSettings) tgt -> do
            linkTarget tgt
        | otherwise -> objectExp bk odata (Link tgt [Plain $ linkTargetToText tgt])
      Link tgt objs -> do
        linkTarget tgt
        content #. expObjs objs
      Timestamp ts -> timestamp ts
      FootnoteRef (FootnoteRefLabel name) -> do
        let def = footnotes Map.!? name
        whenJust def \thing -> do
          content #. either expObjs expEls thing
        "key" #@ name
      FootnoteRef _ -> pass
      Cite _ -> pass -- TODO
      StatisticCookie c ->
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
        "name" #@ name
        "arguments" #. listExp textData args
        content #: macro name args
      InlBabelCall args -> do
        content #: babelCall args
  where
    content = "content"
    expObjs = objectsExp bk odata
    expEls = elementsExp bk odata

elementsExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  [OrgElement] ->
  ExpansionMap m
elementsExp bk odata els = do
  listExp (namespace . elementExp bk odata) els
  "" #* \inner ->
    join <$> forM els \el ->
      callExpansion "org:elements" inner
        `binding` do
          "this" #. elementExp bk odata el

elementDataExp ::
  forall m.
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgElementData ->
  ExpansionMap m
elementDataExp bk odata@OrgData {..} = \case
  (Clock td t) -> do
    tag #@ "clock"
    "timestamp" #. timestamp td
    whenJust t \t' ->
      "duration" #* tsTime t'
  (Paragraph [l@(Link tgt [])])
    | isImgTarget (orgInlineImageRules exporterSettings) tgt -> do
        objectExp bk odata l
        tag #@ "figure"
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
    "query" #. do
      "sections" #* queryExpSecs bk odata el
      "elements" #* queryExpElms bk odata el
      "objects" #* queryExpObjs bk odata el

sectionExp ::
  Monad m =>
  ExportBackend m ->
  OrgData ->
  OrgSection ->
  ExpansionMap m
sectionExp bk odata sec@(OrgSection {..}) = do
  "title" #. objectsExp bk odata sectionTitle
  "tags" #. listExp textData sectionTags
  "children" #. elementsExp bk odata sectionChildren
  "subsections" #. sectionsExp bk odata sectionSubsections
  planning sectionPlanning
  for_ sectionTodo todo
  for_ sectionPriority priority
  "prop" #. mapExp textData sectionProperties
  "anchor" #@ sectionAnchor
  "query" #. do
    "sections" #* queryExpSecs bk odata sec
    "elements" #* queryExpElms bk odata sec
    "objects" #* queryExpObjs bk odata sec
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
sectionsExp bk odata sections = do
  let sections' = groupByLevel sections
  listExp (namespace . sameLevel) sections'
  "" #* \inner ->
    join <$> forM sections' \section ->
      callExpansion "org:sections" inner
        `binding` do
          "this" #. sameLevel section
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
documentExp bk odata doc@(OrgDocument {..}) = do
  "doc" #. do
    "kw" #. keywordsMap bk odata (keywords odata)
    "prop" #. mapExp textData documentProperties
    "children" #. elementsExp bk odata documentChildren
    "sections" #. sectionsExp bk odata documentSections
    "footnotes" #. mapExp fnExp (footnotes odata)
    "tags" #. listExp textData (filetags odata)
    "query" #. do
      "sections" #* queryExpSecs bk odata doc
      "elements" #* queryExpElms bk odata doc
      "objects" #* queryExpObjs bk odata doc
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
      case c of
        [OrgElement _ (Paragraph o)] -> do
          "plain" #. objectsExp bk odata o
        _ -> pass
      where
        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox = "partial"

srcOrExample ::
  forall m.
  Monad m =>
  [SrcLine] ->
  ExpansionMap m
srcOrExample lins = do
  "lines" #. runLines
  "content" #@ srcLinesToText lins
  where
    runLines :: ExpansionMap m
    runLines = listExp (\x -> namespace (lineExps x)) lins

    lineExps (SrcLine c) = do
      "type" #@ "plain"
      "content" #@ c
    lineExps (RefLine i ref c) = do
      "type" #@ "ref"
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
    TimestampData a dt -> do
      dtExps dt
      "type" #@ active a
      "span" #@ "single"
    TimestampRange a dt1 dt2 -> do
      "from" #. dtExps dt1
      "to" #. dtExps dt2
      "type" #@ active a
      "span" #@ "range"
  where
    dtExps :: DateTime -> ExpansionMap m
    dtExps (d, t, r, w) = do
      whenJust r \r' ->
        "repeater" #. tsMark r'
      whenJust w \w' ->
        "warning-period" #. tsMark w'
      "date" #* tsDate d
      whenJust t \t' ->
        "time" #* tsTime t'

    active True = "active"
    active False = "inactive"

    tsMark :: TimestampMark -> ExpansionMap m
    tsMark (_, v, c) = do
      "value" #@ show v
      "unit" #@ one c

    tsDate :: Date -> GlobalExpansion m
    tsDate (y, m, d, _) input = do
      format <- toString <$> lookupAttr' "format" input
      return $
        case ondimCast of
          Just fT -> fT $ toText $ formatTime locale format day
          Nothing -> []
      where
        locale = defaultTimeLocale -- TODO
        day = fromGregorian (toInteger y) m d

tsTime :: Time -> GlobalExpansion m
tsTime (h, m) input = do
  format <- toString <$> lookupAttr' "format" input
  return $
    case ondimCast of
      Just fT -> fT $ toText $ formatTime locale format time
      _ -> []
  where
    locale = defaultTimeLocale -- TODO
    time = TimeOfDay h m 0
