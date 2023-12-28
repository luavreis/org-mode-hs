module Org.Exporters.Processing.GatherAnchors where

import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Ix.Traversable (isequenceA)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core ((%~), (.~), (?~))
import Org.Exporters.Processing.OrgData
import Org.Types.Variants.Annotated

type GatherM = StateT ResolveState M
type GatherT = Compose GatherM

modifyData :: (OrgData -> OrgData) -> GatherM ()
modifyData = lift . modify'

-- | State for resolving links
data ResolveState = ResolveState
  { targetDescriptionCtx :: Maybe OrgObjects
  , srcLineNumber :: Int
  , knownIds :: Set Id
  , idGen :: [Id]
  }
  deriving (Generic)

getUniqueId :: InternalLink Text -> GatherM Id
getUniqueId (internalLinkCanonicalId -> a) = do
  anchors <- gets (.knownIds)
  pure
    if a `Set.member` anchors
      then
        fromMaybe a
          $ find (`Set.notMember` anchors)
          $ map (\n -> a <> "-" <> show (n :: Int)) [1 ..]
      else a

registerFootnote :: FootnoteLabel -> Id -> Either OrgObjects OrgElements -> GatherM ()
registerFootnote name anchor def = do
  modifyData $ #footnotes %~ Map.insert name (anchor, def)

registerAnchorTarget :: InternalLink Text -> Id -> OrgObjects -> GatherM ()
registerAnchorTarget link uid alias = do
  modify' $ #knownIds %~ Set.insert uid
  modifyData $ #internalTargets %~ Map.insert link (uid, alias)

registerKeyword :: Text -> KeywordValue OrgObjects -> GatherM ()
registerKeyword name def =
  modifyData $ #keywords %~ Map.insertWith (<>) name def

withTargetDescription :: OrgObjects -> GatherM a -> GatherM a
withTargetDescription descr x = do
  oldCtx <- gets (.targetDescriptionCtx)
  modify' (#targetDescriptionCtx ?~ descr)
  x <* modify' (#targetDescriptionCtx .~ oldCtx)

-- | Create anchors for sections
resolveSection :: OrgF (GatherT Org) SecIx -> GatherT (OrgF Org) SecIx
resolveSection (OrgSection p a s) = Compose $ do
  s' <- getCompose $ isequenceA s
  let headlineLink = Headline s.rawTitle
  uid <- case Map.lookup "custom_id" s.properties of
    Just cid -> do
      registerAnchorTarget (CustomId cid) cid s'.title
      registerAnchorTarget headlineLink cid s'.title
      return cid
    Nothing -> do
      cid <- getUniqueId headlineLink
      registerAnchorTarget headlineLink cid s'.title
      return cid
  return $ OrgSection p (Aeson.insert "id" (Aeson.String uid) a) s'

-- | Adds context for list items
resolveListItems :: [ListItem (GatherT k) ix] -> GatherM [ListItem k ix]
resolveListItems items =
  flip evalStateT 1 $ forM items \item -> do
    case item.counter of
      Just n0 -> put n0
      Nothing -> modify (+ 1)
    n <- object (StandardProperties 0 0 0) mempty . Plain . show <$> get
    lift $ withTargetDescription n do
      coerce $ isequenceA item

resolveObjects ::
  OrgF (GatherT Org) ObjIx ->
  GatherM (OrgF Org ObjIx)
resolveObjects (OrgObject p a d) =
  OrgObject p a <$> do
    case d of
      FootnoteRef (FootnoteRefDef label def) -> do
        label' <- maybe (getUniqueId (CustomId "anon")) pure label
        let fn = Footnote label'
        anchor <- getUniqueId fn
        def' <- getCompose def
        registerFootnote label' anchor (Left def')
        return $ FootnoteRef $ FootnoteRefLabel label'
      Target name -> do
        anchor <- getUniqueId (Named name)
        desc <- gets (.targetDescriptionCtx)
        registerAnchorTarget (Named name) anchor (fromMaybe mempty desc)
        getCompose $ isequenceA d
      x -> getCompose (isequenceA x)
