module Org.Exporters.Processing.GatherSettings (gatherSettings) where

import Control.Category.Natural (type (~>) (..))
import Data.Ix.Foldable (IFoldable (ifoldMap), ifold)
import Data.Ix.RecursionSchemes qualified as R
import Data.Text qualified as T
import Optics.Core ((%~), (%))
import Org.Exporters.Processing.OrgData
import Org.Types.Variants.Annotated
import Data.Char (isSpace)

getKeywordSetting :: ComposeIx [] OrgF (Const (Ap M ())) ix -> Ap M ()
getKeywordSetting (ComposeIx x) =
  (`foldMap` x) \case
    OrgElement' _ _ _ (Keyword k v)
      | k == "filetags"
      , ValueKeyword tags <- v ->
          Ap $ modify' $ #filetags %~ (++ filter (not . T.null) (T.split (== ':') tags))
      | k == "select_tags"
      , ValueKeyword tags <- v ->
          Ap $ modify' $ #exporterSettings % #orgExportSelectTags %~ (<> fromList (filter (not . T.null) $ T.split isSpace tags))
      | k == "exclude_tags"
      , ValueKeyword tags <- v ->
          Ap $ modify' $ #exporterSettings % #orgExportExcludeTags %~ (<> fromList (filter (not . T.null) $ T.split isSpace tags))
    y -> ifold y

-- Keyword "options" _ -> error "todo"

gatherSettings :: OrgDocument -> M ()
gatherSettings = getAp . ifoldMap (getConst . (R.fold (NT $ Const . getKeywordSetting) #))
