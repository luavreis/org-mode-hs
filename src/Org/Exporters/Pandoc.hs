{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Org.Exporters.Pandoc where
import Org.Types
import Org.Exporters.Common
import Ondim
import Ondim.Pandoc
import Ondim.Extra
import Data.Map.Syntax
import Data.Text qualified as T
import Text.Pandoc (def, readerExtensions, runPure, renderError)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Builder qualified as B
import Relude.Extra.Map
import System.FilePath
import Org.Data.Entities (defaultEntitiesMap, utf8Replacement)
import Paths_org_parser
import System.Directory.Recursive
import Control.Exception (throwIO)

type PTag = PandocTag (State ExporterState)

newtype TemplateLoadingError = TemplateLoadingException String
  deriving (Eq, Show, Exception)

loadBlockTemplates :: IO (OndimS PTag P.Block)
loadBlockTemplates = do
  files <- getFilesRecursive . (</> "templates/md/blocks") =<< getDataDir
  templates <- forM files $ \file -> do
    text :: Text <- decodeUtf8 <$> readFileBS file
    let pandoc = runPure $
          readMarkdown def { readerExtensions = pandocExtensions } text
        name = takeBaseName file
    case pandoc of
      Left s -> throwIO (TemplateLoadingException (toString $ renderError s))
      Right t -> pure (fromString name, blockFromDocument t)
  pure $ OndimS { expansions = fromList templates
                , filters = mempty
                }

loadInlineTemplates :: IO (OndimS PTag P.Inline)
loadInlineTemplates = do
  files <- getFilesRecursive . (</> "templates/md/inlines") =<< getDataDir
  templates <- forM files $ \file -> do
    text :: Text <- decodeUtf8 <$> readFileBS file
    let pandoc = runPure $
          readMarkdown def { readerExtensions = pandocExtensions } text
        name = takeBaseName file
    case pandoc of
      Left s -> throwIO (TemplateLoadingException (toString $ renderError s))
      Right t -> pure (fromString name, inlineFromDocument t)
  pure $ OndimS { expansions = fromList templates
                , filters = mempty
                }

-- renderExpanded ::
--   ExporterSettings ->
--   OrgOndimS ->
--   Expanded HTag -> Either OndimException LByteString
-- renderExpanded exst st spl = spl
--   & withOndimS (const st)
--   & bindDefaults
--   & runOndimT
--   & flip evalState defaultExporterState { exporterSettings = exst }
--   & second toNodeList
--   <&> X.renderHtmlFragment X.UTF8
--   <&> toLazyByteString

-- -- renderExpansible ::
-- --   CanExpand HTag a =>
-- --   ExporterSettings ->
-- --   OrgOndimS ->
-- --   a -> Either OndimException LByteString
-- -- renderExpansible exst st obj =
-- --   renderExpanded exst st (bindDefaults (expand obj))

-- renderDocWithLayout :: OrgOndimS -> X.Document -> OrgDocument -> Either OndimException LByteString
-- renderDocWithLayout st layout doc =
--   expandDocument layout `binding` (documentExpansions doc)
--   & bindDefaults
--   & withOndimS (const st)
--   & runOndimT
--   & flip evalState st'
--   <&> X.render
--   <&> toLazyByteString
--   where
--     st' = defaultExporterState -- todo: get options from the document


documentFilter :: OrgDocument -> Filter PTag P.Pandoc
documentFilter doc x = x
  `binding` do
    "children" ## const $ toList <$> expandOrgElements (documentChildren doc)
    -- "sections" ## const $ expandOrgSections (documentSections doc)
  -- parsedKwExpansions (keywordsFromList $ documentKeywords doc) "meta:"

tags :: HasAttrChild tag t => Tag -> Expansion tag t
tags tag x = children x `bindingText` ("tag" ## pure tag)

expandOrgSections :: [OrgSection] -> Ondim PTag [P.Block]
expandOrgSections [] = pure []
expandOrgSections sections@(OrgSection{sectionLevel = level} : _) = do
  hlevels <- getSetting orgExportHeadlineLevels
  callExpansion "org:sections" (pure P.Null)
    `binding` do
      if level > hlevels
      then switchCases @P.Block "over-level"
      else switchCases @P.Block "normal"
      "sections" ## \x -> mergeLists $
        join <$> forM sections \section ->
          children x
            `binding` do
              "headline-title" ##
                const $ toList <$> expandOrgObjects (sectionTitle section)
              "tags" ## \inner ->
                join <$> forM (sectionTags section) (`tags` inner)
            `binding` do
              "children" ## const $ toList <$> expandOrgElements (sectionChildren section)
              "subsections" ## const $ expandOrgSections (sectionSubsections section)
              "h-n" ## \y -> one . adjLevel level <$> y
            `bindingText` do
              for_ (sectionTodo section) todo
              for_ (sectionPriority section) priority
              "anchor" ## pure $ sectionAnchor section
  where
    adjLevel i (P.Header _ x y) = P.Header i x y
    adjLevel _ x = x
    todo (TodoKeyword st nm) = do
      "todo-state" ## pure (todost st)
      "todo-name" ## pure nm
    todost Done = "done"
    todost Todo = "todo"
    priority p = "priority" ## pure case p of
      (LetterPriority c) -> T.singleton c
      (NumericPriority n) -> show n

expandOrgObjects :: [OrgInline] -> Ondim PTag B.Inlines
expandOrgObjects = foldMapM expandOrgObject

expandOrgObject :: OrgInline -> Ondim PTag B.Inlines
expandOrgObject = \case
  (Plain txt) ->
    pure $ B.text (doSpecialStrings txt) -- TODO
  SoftBreak ->
    pure $ B.softbreak
  LineBreak ->
    pure $ B.linebreak
  (Code txt) ->
    pure $ B.code txt
  (Entity name) ->
    pure $ B.text (maybe "⍰" utf8Replacement (lookup name defaultEntitiesMap))
  (LaTeXFragment InlMathFragment c) ->
    pure $ B.math c
  (LaTeXFragment DispMathFragment c) ->
    pure $ B.displayMath c
  (LaTeXFragment RawFragment c) ->
    pure $ B.rawInline "latex" c
  (ExportSnippet l c) ->
    pure $ B.rawInline l c
  (Src lang _ txt) ->
    pure $ B.codeWith ("", [lang], []) txt
  (Target uid) ->
    pure $ B.spanWith (uid, [], []) mempty
  (Italic objs) ->
    B.emph <$> expandOrgObjects objs
  (Underline objs) ->
    B.underline <$> expandOrgObjects objs
  (Bold objs) ->
    B.strong <$> expandOrgObjects objs
  (Strikethrough objs) ->
    B.strikeout <$> expandOrgObjects objs
  (Superscript objs) ->
    B.superscript <$> expandOrgObjects objs
  (Subscript objs) ->
    B.subscript <$> expandOrgObjects objs
  (Quoted SingleQuote objs) ->
    B.singleQuoted <$> expandOrgObjects objs
  (Quoted DoubleQuote objs) ->
    B.doubleQuoted <$> expandOrgObjects objs
  (Verbatim txt) ->
    call "org:verbatim"
    `bindingText` do "content" ## pure txt
  (Link tgt inl) ->
    call "org:link"
    `bindingText` linkTarget tgt
    `binding` do
      "content" ## const $ toList <$> expandOrgObjects inl
  (Image tgt) ->
    call "org:image"
    `bindingText` linkTarget tgt
  (Timestamp _) ->
    error "TODO"
  (FootnoteRef _) ->
    error "TODO"
  (Cite _) ->
    error "TODO"
  InlBabelCall {} ->
    error "TODO"
  Macro {} ->
    error "TODO"
  where
    call :: Text -> Ondim PTag (B.Inlines)
    call x = B.fromList <$> callExpansion x (pure $ P.Str "")

expandOrgElements :: [OrgElement] -> Ondim PTag B.Blocks
expandOrgElements = foldMapM expandOrgElement

expandOrgElement :: OrgElement -> Ondim PTag B.Blocks
expandOrgElement = \case
  (Paragraph aff [Image tgt]) ->
    call "org:figure"
    `bindingAff` aff
    `bindingText` linkTarget tgt
  (Paragraph aff c) ->
    call "org:paragraph"
    `bindingAff` aff
    `binding` ("content" ## const $ toList <$> expandOrgObjects c)
  (GreaterBlock aff Quote c) ->
    call "org:quote-block"
    `bindingAff` aff
    `binding` content c
  (GreaterBlock aff Center c) ->
    call "org:center-block"
    `bindingAff` aff
    `binding` content c
  (GreaterBlock aff (Special cls) c) ->
    call "org:special-block"
    `bindingAff` aff
    `bindingText` do "special-name" ## pure cls
    `binding` content c
  (PlainList aff k i) ->
    call "org:plain-list"
    `plainList` (k, i)
    `bindingAff` aff
  (DynamicBlock _ _ els) ->
    expandOrgElements els
  (Drawer _ els) ->
    expandOrgElements els
  (ExportBlock l c) ->
    pure $ B.rawBlock l c
  (ExampleBlock aff _ c) ->
    call "org:example-block"
    `bindingAff` aff
    `bindingText` do
      "content" ## pure $ T.intercalate "\n" (srcLineContent <$> c)
  (SrcBlock aff lang _ _ c) ->
    call "org:src-block"
    `bindingAff` aff
    `bindingText` do
      "language" ## pure lang
      "content" ## pure $ T.intercalate "\n" (srcLineContent <$> c)
  (LaTeXEnvironment aff _ text) ->
    call "org:latex-environment"
    `bindingAff` aff
    `bindingText` do "content" ## pure text
  HorizontalRule ->
    pure $ B.horizontalRule
  Keyword {} ->
    pure mempty
  VerseBlock {} ->
    error "TODO"
  Clock {} ->
    error "TODO"
  where
    bindingAff x aff = x
      `binding` affiliatedAttrExpansions "html" aff
      `binding` parsedKwExpansions (fmap toList . expandOrgObjects) aff "kw:"
      `bindingText` textKwExpansions aff "kw:"
    content x = "content" ## const $ toList <$> expandOrgElements x
    call :: Text -> Ondim PTag (B.Blocks)
    call x = B.fromList <$> callExpansion x (pure P.Null)

-- -- sectChildren :: [OrgSection] -> Expanded
-- -- sectChildren childs = do
-- --   hlevels <- getSetting orgExportHeadlineLevels
-- --   flip (maybe (pure [])) (viaNonEmpty (sectionLevel . head) childs) \ level ->
-- --     if level > hlevels
-- --     then one . element "ol" <$> mapM (fmap (element "li") . expand) childs
-- --     else expand childs


plainList :: Ondim PTag t -> (ListType, [ListItem]) -> Ondim PTag t
plainList x (kind, items) = x
  `binding` do
    "list-items" ## listItems
    case kind of
      Ordered OrderedNum -> switchCases "ordered-num"
      Ordered OrderedAlpha -> switchCases "ordered-alpha"
      Descriptive -> switchCases "descriptive"
      Unordered _ -> switchCases "unordered"
  `bindingText`
    case kind of
      Unordered b ->
        "bullet" ## pure (one b)
      _ -> mempty
  where
    listItems :: Expansion PTag P.Block
    listItems inner = adjFstF $ mergeLists $ foldMapM (itemBindings $ children inner) items
      where
        doPlainOrPara [Paragraph _ objs] = B.plain <$> expandOrgObjects objs
        doPlainOrPara els = expandOrgElements els

        start = join $ flip viaNonEmpty items \(ListItem _ i _ _ _ :| _) -> i

        adjFstF :: Filter PTag P.Block
        adjFstF = (map go <$>)
          where go (P.OrderedList (n,y,z) b) = P.OrderedList (fromMaybe n start,y,z) b
                go b = b

        itemBindings y (ListItem _ i cbox t c) = y
          `bindingText` do
            "counter-set" ## pure $ maybe "" show i
            "checkbox" ## pure $ maybe "" checkbox cbox
          `binding` do
            "descriptive-tag" ## const $ toList <$> expandOrgObjects t
          `binding` do
            "list-item-content" ## const $ toList <$> doPlainOrPara c

        checkbox :: Checkbox -> Text
        checkbox (BoolBox True) = "true"
        checkbox (BoolBox False) = "false"
        checkbox PartialBox  = "partial"

mergeLists :: Filter PTag P.Block
mergeLists = (foldr go [] <$>)
  where
    go :: P.Block -> [P.Block] -> [P.Block]
    go (P.BulletList x) (P.BulletList y : r) = P.BulletList (x ++ y) : r
    go (P.OrderedList a x) (P.OrderedList b y : r) | a == b = P.OrderedList a (x ++ y) : r
    go (P.DefinitionList x) (P.DefinitionList y : r) = P.DefinitionList (x ++ y) : r
    go x y = x : y
