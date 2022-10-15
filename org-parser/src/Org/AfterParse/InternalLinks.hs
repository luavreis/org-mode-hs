{-# LANGUAGE RecordWildCards #-}
-- | This module deals with internal link resolution.
module Org.AfterParse.InternalLinks where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Org.Types

type F = Ap (Reader InternalLinkData)

type M = State InternalLinkData

-- | State for doing internal link resolution
data InternalLinkData = InternalLinkData
  { internalTargets :: Map Text (Id, F [OrgObject]),
    targetDescriptionCtx :: Maybe (F [OrgObject]),
    knownAnchors :: Set Id,
    idStack :: [Id]
  }

popUniqueId :: M Text
popUniqueId = do
  ids <- gets idStack
  case ids of
    (x : xs) -> x <$ modify' (\s -> s {idStack = xs})
    [] -> error "something's wrong. out of unique ids"

makeAnchorUnique :: Text -> M Text
makeAnchorUnique a = do
  anchors <- gets knownAnchors
  pure
    if a `Set.member` anchors
      then
        fromMaybe a $
          find (`Set.notMember` anchors) $
            map (\n -> a <> "-" <> show (n :: Int)) [1 ..]
      else a

registerTarget :: Text -> F [OrgObject] -> M Text
registerTarget name alias = do
  targets <- gets internalTargets
  anchors <- gets knownAnchors
  uid <- makeAnchorUnique =<< popUniqueId
  modify' \s ->
    s
      { internalTargets = Map.insert name (uid, alias) targets,
        knownAnchors = Set.insert uid anchors
      }
  pure uid

registerAnchorTarget :: Text -> Text -> F [OrgObject] -> M ()
registerAnchorTarget name anchor alias = do
  targets <- gets internalTargets
  anchors <- gets knownAnchors
  modify' \s ->
    s
      { internalTargets = Map.insert name (anchor, alias) targets,
        knownAnchors = Set.insert anchor anchors
      }

withTargetDescription :: F [OrgObject] -> M a -> M a
withTargetDescription descr =
  withState' \s -> s {targetDescriptionCtx = Just descr}
  where
    withState' f st = state \s -> (evalState st (f s), s)

-- | Create anchors for sections
resolveSection :: OrgSection -> M (F OrgSection)
resolveSection s@OrgSection {..} = do
  title <- sequence <$> mapM resolveObject sectionTitle
  anchor <- case Map.lookup "custom_id" sectionProperties of
    Just a -> do
      registerAnchorTarget ("#" <> a) a title
      registerAnchorTarget ("*" <> titleTxt) a title
      pure a
    Nothing -> do
      a <- makeAnchorUnique $ slugify titleTxt
      registerAnchorTarget ("*" <> titleTxt) a title
      pure a
  return $ s {sectionProperties = Map.insert "custom_id"  }

resolveObject :: OrgObject -> M (F OrgObject)
resolveObject = undefined
