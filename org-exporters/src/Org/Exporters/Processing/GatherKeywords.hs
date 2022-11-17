module Org.Exporters.Processing.GatherKeywords where

import Data.Map qualified as Map
import Org.Exporters.Processing.OrgData
import Org.Types (OrgDocument (..), OrgElement (..))
import Org.Walk

registerKeyword :: Text -> Text -> M ()
registerKeyword name def =
  modify2 \s ->
    pure $ s {keywords = Map.insert name def (keywords s)}

getKeyword :: OrgElement -> Ap M ()
getKeyword = \case
  Keyword "exclude_tags" _ -> error "todo"
  Keyword "select_tags" _ -> error "todo"
  Keyword "options" _ -> error "todo"
  Keyword k v -> Ap $ registerKeyword k v
  _ -> pure ()

gatherKeywords :: OrgDocument -> M ()
gatherKeywords = getAp . query getKeyword
