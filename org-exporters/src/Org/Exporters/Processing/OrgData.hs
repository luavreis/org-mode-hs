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

import Citeproc.CslJson (CslJson (..))
import Data.Aeson qualified as Aeson
import Data.Char (isAlpha)
import Data.Text qualified as T
import Org.Parser (OrgOptions, defaultOrgOptions)
import Org.Types.Variants.Annotated
import System.FilePath (isExtensionOf)
import Text.Slugify (slugify)

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
  , orgExportSelectTags :: Set Text
  -- ^ Tags that select a tree for export.
  --
  -- If any such tag is found, all trees that do not carry one of these tags
  -- will be ignored during export. Inside trees that are selected like this,
  -- you can still deselect a subtree by tagging it with one of the
  -- org-export-exclude-tags.
  --
  -- This option can also be set with the SELECT_TAGS keyword.
  , orgExportExcludeTags :: Set Text
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
  , citeproc :: Bool
  -- ^ Whether to process CSL citations using Citeproc.
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
    , orgExportSelectTags = fromList ["export"]
    , orgExportExcludeTags = fromList ["noexport"]
    , orgExportWithEntities = True
    , orgInlineImageRules = ["png", "jpeg", "svg", "webp", "gif", "jpg"]
    , orgLinkAbbrevAlist = mempty
    , headlineLevelShift = 0
    , citeproc = True
    }

-- | FIXME This is not exactly how org figures out if a link is an image.
isImgTarget :: [FilePath] -> LinkTarget -> Bool
isImgTarget exts = \case
  (URILink _ x) -> hasImgExtension x
  (UnresolvedLink x) -> hasImgExtension x
  where
    hasImgExtension x = any (`isExtensionOf` toString x) exts

-- | Takes a raw title string and produces a suitable anchor.
sectionTitleToAnchor :: Text -> Text
sectionTitleToAnchor = T.dropWhile (not . isAlpha) . slugify

internalLinkCanonicalId :: InternalLink Text -> Id
internalLinkCanonicalId = \case
  CustomId a -> a
  Headline a -> sectionTitleToAnchor a
  Footnote a -> "fn-" <> T.dropWhile (not . isAlpha) a
  Named a -> T.dropWhile (not . isAlpha) a

type Id = Text
type ReferrerId = Id
type FootnoteLabel = Text

-- | Metadata associated with the document
data OrgData = OrgData
  { keywords :: Keywords OrgObjects
  , filetags :: [Text]
  , exporterSettings :: ExporterSettings
  , parserOptions :: OrgOptions
  , internalTargets :: Map (InternalLink Text) (Id, OrgObjects)
  , footnotes :: Map FootnoteLabel (Id, Either OrgObjects OrgElements)
  , bibliography :: [(Text, CslJson Text)]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

initialOrgData :: OrgData
initialOrgData = OrgData mempty [] defaultExporterSettings defaultOrgOptions mempty mempty []

type M = State OrgData
type T = Compose M
