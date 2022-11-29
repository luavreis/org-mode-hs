module Org.Exporters.Processing.GatherKeywords where

import Data.Map qualified as Map
import Org.Exporters.Processing.OrgData
import Data.Text qualified as T
import Org.Parser (evalOrgMaybe)
import Org.Parser.Objects (plainMarkupContext, standardSet)
import Org.Types (OrgDocument (..), OrgElement (..))
import Org.Walk

registerFiletags :: Text -> M ()
registerFiletags tags = do
  modify2 \s ->
    return s { filetags = filetags s ++ filter (not . T.null) (T.split (== ':') tags) }

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
  Keyword k v -> Ap do
    registerKeyword k v
    case k of
      "title" -> registerTitle v
      "filetags" -> registerFiletags v
      _ -> pure ()
  _ -> pure ()

gatherKeywords :: OrgDocument -> M ()
gatherKeywords = getAp . query getKeyword
