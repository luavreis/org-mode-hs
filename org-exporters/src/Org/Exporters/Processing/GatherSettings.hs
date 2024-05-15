module Org.Exporters.Processing.GatherSettings (gatherSettings) where

import Data.Char (isSpace)
import Data.Ix.Foldable (IFoldable (..))
import Data.Ix.Functor (IFunctor)
import Data.Text qualified as T
import Optics.Core ((%), (%~))
import Org.Exporters.Settings (ExporterSettings)
import Org.Types.Variants.Basic
import Org.Types.Walk (queryTopDown)

getKeywordSetting :: OrgF k ix -> Endo ExporterSettings
getKeywordSetting =
  Endo . \case
    (OrgElementF _ (Keyword k (ValueKeyword tags)))
      | k == "filetags" -> #filetags %~ (++ splitOpts (== ':') tags)
      | k == "select_tags" -> #orgExportSelectTags %~ (fromList (splitOpts isSpace tags) <>)
      | k == "exclude_tags" -> #orgExportExcludeTags %~ (fromList (splitOpts isSpace tags) <>)
    _other -> id
  where
    splitOpts p str = filter (not . T.null) $ T.split p str

gatherSettings :: (IFoldable f, IFunctor f) => OrgDocumentData (Org f) i -> Endo ExporterSettings
gatherSettings = ifoldMap (queryTopDown getKeywordSetting)
