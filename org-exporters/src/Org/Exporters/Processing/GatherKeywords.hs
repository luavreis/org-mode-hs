module Org.Exporters.Processing.GatherKeywords where

import Data.Map qualified as Map
import Org.Exporters.Processing.OrgData
import Org.Parser (evalOrgMaybe)
import Org.Parser.Objects (plainMarkupContext, standardSet)
import Org.Types (OrgDocument (..), OrgElement (..))
import Org.Walk

registerTitle :: Text -> M ()
registerTitle title = do
  modify2 \s -> do
    opts <- asks parserOptions
    let parser = plainMarkupContext standardSet
    return case evalOrgMaybe opts parser title of
      Just title' -> s {parsedTitle = toList title'}
      Nothing -> s

registerKeyword :: Text -> Text -> M ()
registerKeyword name def =
  modify2 \s ->
    pure $
      s
        { keywords =
            Map.insertWith
              (\x y -> x <> "\n" <> y)
              name
              def
              (keywords s)
        }

getKeyword :: OrgElement -> Ap M ()
getKeyword = \case
  -- Keyword "exclude_tags" _ -> error "todo" TODO
  -- Keyword "select_tags" _ -> error "todo"
  -- Keyword "options" _ -> error "todo"
  Keyword "title" v -> Ap $ registerTitle v
  Keyword k v -> Ap $ registerKeyword k v
  _ -> pure ()

gatherKeywords :: OrgDocument -> M ()
gatherKeywords = getAp . query getKeyword
