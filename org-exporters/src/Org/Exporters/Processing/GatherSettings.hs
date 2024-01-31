module Org.Exporters.Processing.GatherSettings (gatherSettings) where

import Control.Category.Natural (type (~>) (..))
import Data.Char (isSpace)
import Data.Ix.Foldable (ifoldMap, ifold)
import Data.Ix.RecursionSchemes qualified as R
import Data.Text qualified as T
import Optics.Core ((%), (%~))
import Org.Exporters.Processing.OrgData
import Org.Types.Variants.Annotated

getKeywordSetting :: ComposeIx [] OrgF (Const (Ap M ())) ix -> Ap M ()
getKeywordSetting (ComposeIx x) =
  (`foldMap` x) \case
    (OrgElement' _ _ _ (Keyword k v)) -> Ap $ case k of
      "filetags"
        | ValueKeyword tags <- v ->
            modify' $ #filetags %~ (++ filter (not . T.null) (T.split (== ':') tags))
      "select_tags"
        | ValueKeyword tags <- v ->
            modify' $ #exporterSettings % #orgExportSelectTags %~ (<> fromList (filter (not . T.null) $ T.split isSpace tags))
      "exclude_tags"
        | ValueKeyword tags <- v ->
            modify' $ #exporterSettings % #orgExportExcludeTags %~ (<> fromList (filter (not . T.null) $ T.split isSpace tags))
      _ -> pass
    y -> ifold y

-- Keyword "options" _ -> error "todo"

gatherSettings :: OrgDocument -> M ()
gatherSettings = getAp . ifoldMap (getConst . (R.fold (NT $ Const . getKeywordSetting) #))
