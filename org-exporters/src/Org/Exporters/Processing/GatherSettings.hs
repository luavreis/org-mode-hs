module Org.Exporters.Processing.GatherSettings where

import Data.Text qualified as T
import Org.Exporters.Processing.OrgData
import Org.Types (KeywordValue (..), OrgDocument (..), OrgElementData (..))
import Org.Walk

registerFiletags :: KeywordValue -> M ()
registerFiletags = \case
  ValueKeyword tags ->
    modify2 \s ->
      return s {filetags = filetags s ++ filter (not . T.null) (T.split (== ':') tags)}
  _ -> pure ()

getKeywordSetting :: OrgElementData -> Ap M ()
getKeywordSetting = \case
  -- Keyword "exclude_tags" _ -> error "todo" TODO
  -- Keyword "select_tags" _ -> error "todo"
  -- Keyword "options" _ -> error "todo"
  Keyword k v -> Ap do
    case k of
      "filetags" -> registerFiletags v
      _ -> pure ()
  _ -> pure ()

gatherSettings :: OrgDocument -> M ()
gatherSettings = getAp . query getKeywordSetting
