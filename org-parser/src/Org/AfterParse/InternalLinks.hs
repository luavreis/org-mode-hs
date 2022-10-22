{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | This module deals with internal link resolution.
module Org.AfterParse.InternalLinks where

import Control.Monad.Writer.Strict -- TODO: change to CPS when relude updates
import Control.MultiWalk
import Data.Map qualified as Map
import Data.Set qualified as Set
import Org.Types
import Org.Walk
import Text.Slugify (slugify)

type F = Reader OrgData

type M = WriterT OrgData (State ResolveState)

-- | State for doing internal link resolution
data ResolveState = ResolveState
  { targetDescriptionCtx :: Maybe [OrgObject],
    srcLineNumber :: Int,
    knownAnchors :: Set Id,
    idStack :: [Id]
  }

initialResolveState :: ResolveState
initialResolveState = ResolveState
  { targetDescriptionCtx = mempty,
    srcLineNumber = 0,
    knownAnchors = mempty,
    idStack = [show i | (i :: Int) <- [1..]]
  }

-- | Metadata associated with the document
newtype OrgData = OrgData
  { internalTargets :: Map Text (Id, F [OrgObject])
  }
  deriving (Semigroup, Monoid)

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
  anchors <- gets knownAnchors
  uid <- makeAnchorUnique =<< popUniqueId
  modify' \s -> s {knownAnchors = Set.insert uid anchors}
  tell (OrgData $ Map.singleton name (uid, alias))
  pure uid

registerAnchorTarget :: Text -> Text -> F [OrgObject] -> M ()
registerAnchorTarget name anchor alias = do
  anchors <- gets knownAnchors
  modify' \s -> s {knownAnchors = Set.insert anchor anchors}
  tell (OrgData $ Map.singleton name (anchor, alias))

withTargetDescription :: [OrgObject] -> M a -> M a
withTargetDescription descr x = do
  oldCtx <- gets targetDescriptionCtx
  modify' \s -> s {targetDescriptionCtx = Just descr}
  x <* modify' \s -> s {targetDescriptionCtx = oldCtx}

-- | Create anchors for sections
resolveSection :: Walk MWTag (Compose M F) -> OrgSection -> Compose M F OrgSection
resolveSection f s@OrgSection {..} = Compose $ do
  title <- getCompose $ traverse f sectionTitle
  child <- getCompose $ traverse f sectionChildren
  anchor <- case Map.lookup "custom_id" sectionProperties of
    Just a -> do
      registerAnchorTarget ("#" <> a) a title
      registerAnchorTarget ("*" <> sectionRawTitle) a title
      pure a
    Nothing -> do
      a <- makeAnchorUnique $ slugify sectionRawTitle
      registerAnchorTarget ("*" <> sectionRawTitle) a title
      pure a
  return do
    title' <- title
    child' <- child
    return s {sectionTitle = title', sectionChildren = child', sectionAnchor = anchor}

resolveListItems :: Walk MWTag (Compose M F) -> [ListItem] -> Compose M F [ListItem]
resolveListItems f items =
  Compose $
    flip evalStateT 1 $
      sequence <$> forM items \(ListItem b i c t e) -> do
        case i of
          Just n0 -> put n0
          Nothing -> modify (+ 1)
        n <- one . Plain . show <$> get
        lift $ withTargetDescription n do
          tag <- getCompose $ traverse f t
          els <- getCompose $ traverse f e
          return $ ListItem b i c <$> tag <*> els

resolve :: Walk MWTag (Compose M F)
resolve = buildMultiW @MWTag \_ l ->
  l .> resolveSection resolve
    .> resolveListItems resolve

fullResolve :: MultiWalk MWTag t => t -> t
fullResolve x =
  let (resolved, datum) =
        evalState (runWriterT $ getCompose $ resolve x) initialResolveState
  in runReader resolved datum
