{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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

import Data.Ix.RecursionSchemes (Fix (..))
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (TimeOfDay (..), defaultTimeLocale, formatTime, fromGregorian)
import Ondim
import Ondim.Extra.BindJSON qualified as O
import Ondim.Extra.Expansions (assocsExp, ifElse, listExp, lookupAttr', mapExp)
import Org.Data.Entities qualified as Data
import Org.Exporters.Processing
import Org.Parser (OrgParser, parseOrgMaybe)
import Org.Parser.Elements (elements)
import Org.Parser.Objects (plainMarkupContext, standardSet)
import Org.Types.Variants.Annotated
import Org.Types.Variants.ParseInfo qualified as PI
import System.FilePath (isRelative, takeExtension, (-<.>))

data ExportBackend m = ExportBackend
  { affiliatedMap :: Keywords OrgObjects -> ExpansionMap m
  , macro :: Text -> [Text] -> SomeExpansion m
  , babelCall :: BabelCall -> SomeExpansion m
  , srcPretty :: Keywords OrgObjects -> Text -> Text -> SomeExpansion m
  , customExp :: forall ix. ExportBackend m -> OrgData -> OrgF Org ix -> Maybe (ExpansionMap m)
  }

keywordsMap ::
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  Keywords OrgObjects ->
  ExpansionMap m
keywordsMap bk odata = mapExp (kwValueExp bk odata)

kwValueExp ::
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  KeywordValue OrgObjects ->
  SomeExpansion m
kwValueExp bk odata = \case
  ParsedKeyword t -> namespace $ objectsExp bk odata t
  ValueKeyword t -> textData t
  BackendKeyword t -> namespace $ assocsExp textData t

-- | Text expansion for link target.
linkTarget ::
  (Monad m) =>
  LinkTarget ->
  ExpansionMap m
linkTarget tgt = do
  "target" #@ case tgt of
    URILink "file" (changeExtension -> file)
      | isRelative file -> toText file
      | otherwise -> "file:///" <> T.dropWhile (== '/') (toText file)
    URILink scheme uri -> scheme <> ":" <> uri
    AnchorLink anchor -> "#" <> anchor
    UnresolvedLink other -> other
  case tgt of
    URILink scheme uri -> do
      "scheme" #@ scheme
      "path" #@ uri
      "extension" #@ uri
    _ -> pure ()
  where
    changeExtension (toString -> file) =
      if takeExtension file == ".org"
        then file -<.> ".html"
        else file

parserExp ::
  (Monad m) =>
  OrgData ->
  OrgParser (PI.Org ix) ->
  (Org ix -> ExpansionMap m) ->
  GlobalExpansion m
parserExp odata parser expand self = do
  txt <- fromMaybe "" <$> lookupAttr "text" self
  case parseOrgMaybe odata.parserOptions parser txt of
    Just parsed ->
      let solved = evalState (gatherAnchors >=> resolveLinks $ convert parsed) odata
       in liftChildren self
            `binding` expand solved
    Nothing -> throwTemplateError $ "Could not parse " <> show txt

parserExpObjs ::
  ExportBackend m ->
  OrgData ->
  GlobalExpansion m
parserExpObjs bk odata =
  parserExp odata (plainMarkupContext standardSet) (objectsExp bk odata)

parserExpElms ::
  ExportBackend m ->
  OrgData ->
  GlobalExpansion m
parserExpElms bk odata =
  parserExp odata elements (elementsExp bk odata)

-- type GeneralizeQuery a = forall m. Monoid m => Generalize (Org ~> Const m) (a -> m) :: Constraint

queryExp ::
  forall sx m a.
  ( Monad m
  , SingI sx
  , GeneralizeQuery Org a
  ) =>
  -- | How to render the query results.
  (Org sx -> ExpansionMap m) ->
  -- | How to select the results (the attributes are from the caller's node).
  ([Attribute] -> OrgF Org sx -> Bool) ->
  -- | Data to query for results.
  a ->
  GlobalExpansion m
queryExp g f thing self = do
  attrs <- attributes self
  let queryf :: forall jx. Org jx -> Org sx
      queryf = coerce $ mapMaybe (getsIx (const Nothing) (sing @sx) (guarded (f attrs)))
      query =
        if any ((== "bottom-up") . fst) attrs
          then queryBottomUp
          else queryTopDown
      result = query queryf thing
  liftChildren self
    `binding` do
      "result" #. g result

queryExpObjs ::
  ( Monad m
  , GeneralizeQuery Org a
  ) =>
  ExportBackend m ->
  OrgData ->
  a ->
  GlobalExpansion m
queryExpObjs bk odata =
  queryExp (objectsExp bk odata) \attrs (OrgObject props ann d) ->
    case L.lookup "type" attrs of
      Just tag
        | objectTag odata d == tag -> True
      _ -> False

queryExpElms ::
  ( Monad m
  , GeneralizeQuery Org a
  ) =>
  ExportBackend m ->
  OrgData ->
  a ->
  GlobalExpansion m
queryExpElms bk odata =
  queryExp (elementsExp bk odata) \attrs (OrgElement props ann kws _) ->
    let go (x, y) = (,y) <$> T.stripPrefix "kw:" x
        kwFils = mapMaybe go attrs
        gop (x, y) p = (Just (ValueKeyword y) == Map.lookup x kws) && p
     in foldr gop True kwFils

queryExpSecs ::
  ( Monad m
  , GeneralizeQuery Org a
  ) =>
  ExportBackend m ->
  OrgData ->
  a ->
  GlobalExpansion m
queryExpSecs bk odata =
  queryExp (sectionsExp bk odata) \attrs (OrgSection props ann s@OrgSectionData {}) ->
    let go (x, y) = (,y) <$> T.stripPrefix "prop:" x
        todoNm = maybe True (((.name) <$> s.todo ==) . Just) (L.lookup "todo-name" attrs)
        todoStName Todo = "todo"
        todoStName Done = "done"
        todoSt = maybe True ((todoStName . (.state) <$> s.todo ==) . Just) (L.lookup "todo-state" attrs)
        level = maybe True (s.level ==) (readMaybe . toString =<< L.lookup "level" attrs)
        kwFils = mapMaybe go attrs
        gop (x, y) p = (Just y == Map.lookup x s.properties) && p
     in foldr gop True kwFils && todoNm && todoSt && level

queryExps :: (Monad m, GeneralizeQuery Org a) => ExportBackend m -> OrgData -> a -> ExpansionMap m
queryExps bk odata x =
  "query" #. do
    "sections" #* queryExpSecs bk odata x
    "elements" #* queryExpElms bk odata x
    "objects" #* queryExpObjs bk odata x

objectTag :: OrgData -> OrgObjectData Org ix -> Text
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
  Link tgt (coerce -> [] @(OrgF Org _))
    | isImgTarget exporterSettings.orgInlineImageRules tgt -> "image"
  Link {} -> "link"
  Timestamp {} -> "timestamp"
  FootnoteRef {} -> "footnote-ref"
  Cite {} -> "cite"
  StatisticCookie {} -> "statistic-cookie"
  Macro {} -> "macro"
  InlBabelCall {} -> "babel-call"

objectsExp ::
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgObjects ->
  ExpansionMap m
objectsExp bk odata objs = do
  queryExps bk odata objs
  listExp (namespace . objectExp bk odata) (coerce objs)
  "" #* \inner ->
    join <$> forM (coerce objs) \obj ->
      callExpansion "org:objects" inner
        `binding` do
          "this" #. objectExp bk odata obj

objectExp ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgF Org ObjIx ->
  ExpansionMap m
objectExp bk odata obj@(OrgObject props ann objdata) = do
  -- (`fromMaybe` customElement bk odata el) do
  queryExps bk odata obj
  O.objectExp ann
  objectDataExp bk odata objdata

objectDataExp ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgObjectData Org ObjIx ->
  ExpansionMap m
objectDataExp bk@ExportBackend {..} odata@OrgData {..} obj = do
  "tag" #@ objectTag odata obj
  case obj of
    Plain txt -> do
      let specialStrings = exporterSettings.orgExportWithSpecialStrings
      content #@ if specialStrings then doSpecialStrings txt else txt
    LineBreak -> pass
    Code txt -> content #@ txt
    Entity name
      | exporterSettings.orgExportWithEntities -> do
          let Data.Entity {..} = Data.defaultEntitiesMap Map.! name
          "if-math" #* ifElse latexMathP
          "latex" #@ latexReplacement
          "ascii" #@ asciiReplacement
          "html" #@ htmlReplacement
          "latin" #@ latin1Replacement
          "utf8" #@ utf8Replacement
      | otherwise -> objectDataExp bk odata (Plain $ "\\" <> name)
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
    Target name -> do
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
    Link tgt (coerce -> [] @(OrgF Org _))
      | isImgTarget exporterSettings.orgInlineImageRules tgt -> do
          linkTarget tgt
      | otherwise -> objectDataExp bk odata (Link tgt (object (StandardProperties 0 0 0) mempty (Plain $ linkTargetToText tgt)))
    Link tgt objs -> do
      linkTarget tgt
      content #. expObjs objs
    Timestamp ts -> timestamp ts
    FootnoteRef (FootnoteRefLabel name) -> do
      let def = footnotes Map.!? name
      whenJust def \(target, thing) -> do
        content #. either expObjs expEls thing
        "target" #@ target
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
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgElements ->
  ExpansionMap m
elementsExp bk odata els = do
  queryExps bk odata els
  listExp (namespace . elementExp bk odata) (coerce els)
  "" #* \inner ->
    join <$> forM (coerce els) \el ->
      callExpansion "org:elements" inner
        `binding` do
          "this" #. elementExp bk odata el

elementExp ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgF Org ElmIx ->
  ExpansionMap m
elementExp bk@ExportBackend {..} odata el@(OrgElement props anns aff eldata) = do
  -- (`fromMaybe` customElement bk odata el) do
  queryExps bk odata el
  O.objectExp anns
  elementDataExp bk odata eldata
  affiliatedMap aff
  "akw" #. keywordsMap bk odata aff

elementDataExp ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgElementData Org ElmIx ->
  ExpansionMap m
elementDataExp bk odata@OrgData {..} = \case
  (Clock td t) -> do
    tag #@ "clock"
    "timestamp" #. timestamp td
    whenJust t \t' ->
      "duration" #* tsTime t'
  (Paragraph (coerce -> [l@(OrgObject _ _ (Link tgt desc))]))
    | desc == mempty
    , isImgTarget exporterSettings.orgInlineImageRules tgt -> do
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

sectionsExp ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgSections ->
  ExpansionMap m
sectionsExp bk odata secs@(coerce -> sections) = do
  queryExps bk odata secs
  let sections' = groupByLevel sections
  listExp (namespace . sameLevel) sections'
  "" #* \inner ->
    join <$> forM sections' \s ->
      callExpansion "org:sections" inner
        `binding` do
          "this" #. sameLevel s
  where
    hlevels = odata.exporterSettings.orgExportHeadlineLevels
    shift = odata.exporterSettings.headlineLevelShift
    sameLevel (level, sameLvlSecs) = do
      "type" #@ if level + shift > hlevels then "over-level" else "normal"
      "level" #@ show $ level + shift
      listExp (namespace . sectionExp bk odata) sameLvlSecs

    groupByLevel :: [OrgF Org SecIx] -> [(Int, [OrgF Org SecIx])]
    groupByLevel = foldr go []
      where
        go s [] = [(s.datum.datum.level, [s])]
        go s l@((i, ss) : ls)
          | s.datum.datum.level == i = (i, s : ss) : ls
          | otherwise = (s.datum.datum.level, [s]) : l

sectionExp ::
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgF Org SecIx ->
  ExpansionMap m
sectionExp bk odata s@(OrgSection props anns datum) = do
  queryExps bk odata s
  sectionDataExp bk odata datum
  O.objectExp anns

sectionDataExp ::
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  OrgSectionData Org SecIx ->
  ExpansionMap m
sectionDataExp bk odata s = do
  "title" #. objectsExp bk odata s.title
  "tags" #. listExp textData s.tags
  "children" #. elementsExp bk odata s.children
  "subsections" #. sectionsExp bk odata s.subsections
  planning s.planning
  for_ s.todo todo
  for_ s.priority priority
  "prop" #. mapExp textData s.properties
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
    planning info =
      let assocs =
            catMaybes
              [ ("closed",) <$> info.closed
              , ("deadline",) <$> info.deadline
              , ("scheduled",) <$> info.scheduled
              ]
          renderTs a = namespace $ objectDataExp bk odata (Timestamp a)
       in "planning" #. assocsExp renderTs assocs

documentExp ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  -- | Prefix for expansion names
  OrgData ->
  OrgDocument ->
  ExpansionMap m
documentExp bk odata doc = do
  "doc" #. do
    "kw" #. keywordsMap bk odata odata.keywords
    "prop" #. mapExp textData doc.properties
    "children" #. elementsExp bk odata doc.children
    "sections" #. sectionsExp bk odata doc.sections
    "footnotes" #. mapExp footnoteExp (fmap snd odata.footnotes)
    "tags" #. listExp textData odata.filetags
    "query" #. do
      "sections" #* queryExpSecs bk odata doc
      "elements" #* queryExpElms bk odata doc
      "objects" #* queryExpObjs bk odata doc
  where
    footnoteExp =
      namespace
        . elementsExp bk odata
        . either (element (StandardProperties 0 0 0) mempty mempty . Paragraph) id

table ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  [TableRow OrgObjects] ->
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

    tableCell :: (Maybe ColumnAlignment, Org ObjIx) -> ExpansionMap m
    tableCell (alig, cell) = do
      "content" #. objectsExp bk odata cell
      for_ alig \a ->
        "alignment" #@ case a of
          AlignLeft -> "left"
          AlignRight -> "right"
          AlignCenter -> "center"

    alignment =
      (++ repeat Nothing)
        $ fromMaybe []
        $ listToMaybe props

plainList ::
  forall m.
  (Monad m) =>
  ExportBackend m ->
  OrgData ->
  ListType ->
  [ListItem Org ElmIx] ->
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
    listItemExp :: ListItem Org ElmIx -> ExpansionMap m
    listItemExp (ListItem _ i cbox t c) = do
      for_ i \i' -> "counter-set" #@ show i'
      for_ cbox \cbox' -> "checkbox" #@ checkbox cbox'
      whenJust t \t' ->
        "descriptive-tag" #. objectsExp bk odata t'
      "content" #. elementsExp bk odata c
      case coerce c of
        [OrgElement _ _ _ (Paragraph o)] -> do
          "plain" #. objectsExp bk odata o
        _ -> pass
      where
        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox = "partial"

srcOrExample ::
  forall m.
  (Monad m) =>
  [SrcLine] ->
  ExpansionMap m
srcOrExample lins = do
  "lines" #. runLines
  "content" #@ srcLinesToText lins
  where
    runLines :: ExpansionMap m
    runLines = listExp (\x -> namespace ("content" #@ x)) lins

timestamp ::
  forall m.
  (Monad m) =>
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
    dtExps :: OrgDateTime -> ExpansionMap m
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

    tsDate :: OrgDate -> GlobalExpansion m
    tsDate date input = do
      format <- toString <$> lookupAttr' "format" input
      return
        $ case ondimCast of
          Just fT -> fT $ toText $ formatTime locale format day
          Nothing -> []
      where
        locale = defaultTimeLocale -- TODO
        day = fromGregorian (toInteger date.year) date.month date.day

tsTime :: OrgTime -> GlobalExpansion m
tsTime time input = do
  format <- toString <$> lookupAttr' "format" input
  return
    $ case ondimCast of
      Just fT -> fT $ toText $ formatTime locale format time'
      _ -> []
  where
    locale = defaultTimeLocale -- TODO
    time' = TimeOfDay time.hour time.minute 0
