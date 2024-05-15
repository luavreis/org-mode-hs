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
import System.FilePath (isExtensionOf)
import Text.Slugify (slugify)

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
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)

-- FIXME upstream
deriving instance (Generic (CslJson Text))
deriving instance (NFData (CslJson Text))

initialOrgData :: OrgData
initialOrgData = OrgData mempty [] defaultExporterSettings defaultOrgOptions mempty mempty []

type M = State OrgData
type T = Compose M
