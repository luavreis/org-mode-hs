{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module deals with internal link resolution.
module Org.AfterParse.InternalLinks where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Org.Types
import Org.Walk
import Control.Monad.Writer.Strict -- TODO: change to CPS when relude updates
import Text.Slugify (slugify)

type F = Reader OrgData

type M = WriterT OrgData (State ResolveState)

-- | State for doing internal link resolution
data ResolveState = ResolveState
  { targetDescriptionCtx :: Maybe [OrgObject],
    knownAnchors :: Set Id,
    idStack :: [Id]
  }

-- | State for doing internal link resolution
newtype OrgData = OrgData
  { internalTargets :: Map Text (Id, [OrgObject])
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

registerTarget :: Text -> [OrgObject] -> M Text
registerTarget name alias = do
  anchors <- gets knownAnchors
  uid <- makeAnchorUnique =<< popUniqueId
  modify' \s -> s {knownAnchors = Set.insert uid anchors}
  tell (OrgData $ Map.singleton name (uid, alias))
  pure uid

registerAnchorTarget :: Text -> Text -> [OrgObject] -> M ()
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
resolveSection :: OrgSection -> M OrgSection
resolveSection s@OrgSection {..} = do
  title <- walkM resolveObject sectionTitle
  child <- mapM resolveElement sectionChildren
  anchor <- case Map.lookup "custom_id" sectionProperties of
    Just a -> do
      registerAnchorTarget ("#" <> a) a title
      registerAnchorTarget ("*" <> sectionRawTitle) a title
      pure a
    Nothing -> do
      a <- makeAnchorUnique $ slugify sectionRawTitle
      registerAnchorTarget ("*" <> sectionRawTitle) a title
      pure a
  return s {sectionTitle = title, sectionChildren = child, sectionAnchor = anchor}

resolveElement :: OrgElement -> M OrgElement
resolveElement = \case
  s@PlainList {..} -> do
    aff <- walkM resolveObject affKws
    items <-
      flip evalStateT 1 $
        forM listItems \(ListItem b i c t e) -> do
          case i of
            Just n0 -> put n0
            Nothing -> modify (+ 1)
          n <- one . Plain . show <$> get
          lift $ withTargetDescription n do
            tag <- mapM resolveObject t
            els <- mapM resolveElement e
            return $ ListItem b i c tag els
    return s {affKws = aff, listItems = items}
  e -> walkElementM resolveElement e

-- items <- flip evalStateT 0 $ undefined
-- return do
--   aff' <- aff
--   items' <- items
--   s
--     { affKws = aff'
--     }

resolveObject :: OrgObject -> M OrgObject
resolveObject = undefined
