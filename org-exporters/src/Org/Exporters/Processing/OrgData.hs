{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Org.Exporters.Processing.OrgData
  ( module Org.Exporters.Processing.OrgData
  )
where

import Data.Aeson qualified as Aeson
import Org.Parser (OrgOptions, defaultOrgOptions)
import Org.Types
import System.FilePath (isExtensionOf)

data ExporterSettings = ExporterSettings
  { orgExportHeadlineLevels :: Int
  -- ^ The last level which is still exported as a headline.
  --
  -- Inferior levels will usually produce itemize or enumerate lists when
  -- exported, but back-end behavior may differ.
  --
  -- This option can also be set with the OPTIONS keyword,
  -- e.g. "H:2".
  , orgExportWithSpecialStrings :: Bool
  -- ^ Interpret "\-", "--", "---" and "..." for export.
  --
  -- This option can also be set with the OPTIONS keyword,
  -- e.g. "-:nil".
  , orgExportSelectTags :: [Text]
  -- ^ Tags that select a tree for export.
  --
  -- If any such tag is found, all trees that do not carry one of these tags
  -- will be ignored during export. Inside trees that are selected like this,
  -- you can still deselect a subtree by tagging it with one of the
  -- org-export-exclude-tags.
  --
  -- This option can also be set with the SELECT_TAGS keyword.
  , orgExportExcludeTags :: [Text]
  -- ^ Tags that exclude a tree from export.
  --
  -- All trees carrying any of these tags will be excluded from export. This
  -- is without condition, so even subtrees inside that carry one of the
  -- org-export-select-tags will be removed.
  --
  -- This option can also be set with the EXCLUDE_TAGS keyword.
  , orgExportWithEntities :: Bool
  -- ^ Interpret entities when exporting.
  --
  -- This option can also be set with the OPTIONS keyword,
  -- e.g. "e:nil".
  , orgInlineImageRules :: [String]
  -- ^ See Org Mode's `org-html-inline-image-rules` and friends.
  --
  -- This is a list of link extensions that should be inlined as images.
  -- Basically, it just affects figures.
  , orgLinkAbbrevAlist :: Map Text Text
  -- ^ See <https://orgmode.org/manual/Link-Abbreviations.html>
  , headlineLevelShift :: Int
  -- ^ Global shift of headline levels.
  }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)

instance Aeson.ToJSON ExporterSettings where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance Aeson.FromJSON ExporterSettings where
  parseJSON = Aeson.genericParseJSON aesonOptions

aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '-'
    }

defaultExporterSettings :: ExporterSettings
defaultExporterSettings =
  ExporterSettings
    { orgExportHeadlineLevels = 3
    , orgExportWithSpecialStrings = True
    , orgExportSelectTags = ["export"]
    , orgExportExcludeTags = ["noexport"]
    , orgExportWithEntities = True
    , orgInlineImageRules = ["png", "jpeg", "svg", "webp", "gif", "jpg"]
    , orgLinkAbbrevAlist = mempty
    , headlineLevelShift = 0
    }

-- | FIXME This is not exactly how org figures out if a link is an image.
isImgTarget :: [FilePath] -> LinkTarget -> Bool
isImgTarget exts = \case
  (URILink _ x) -> hasImgExtension x
  (UnresolvedLink x) -> hasImgExtension x
  _ -> False
  where
    hasImgExtension x = any (`isExtensionOf` toString x) exts

-- | Metadata associated with the document
data OrgData = OrgData
  { keywords :: Keywords
  , filetags :: [Text]
  , exporterSettings :: ExporterSettings
  , parserOptions :: OrgOptions
  , internalTargets :: Map Text (Id, [OrgObject])
  , footnotes :: Map Text (Either [OrgObject] [OrgElement])
  }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)

initialOrgData :: OrgData
initialOrgData = OrgData mempty [] defaultExporterSettings defaultOrgOptions mempty mempty

type F = Reader OrgData

type M = State (ResolveState, F OrgData)

-- | State for doing AST post-processing
data ResolveState = ResolveState
  { targetDescriptionCtx :: Maybe [OrgObject]
  , srcLineNumber :: Int
  , knownAnchors :: Set Id
  , idStack :: [Id]
  }

initialResolveState :: ResolveState
initialResolveState =
  ResolveState
    { targetDescriptionCtx = mempty
    , srcLineNumber = 0
    , knownAnchors = mempty
    , idStack = [show i | (i :: Int) <- [1 ..]]
    }

gets1 :: MonadState (s, t) m => (s -> a) -> m a
gets1 = gets . (. fst)

modify1 :: MonadState (s, t) m => (s -> s) -> m ()
modify1 = modify' . first

modify2 :: (OrgData -> F OrgData) -> M ()
modify2 f = modify' $ second (>>= f)
