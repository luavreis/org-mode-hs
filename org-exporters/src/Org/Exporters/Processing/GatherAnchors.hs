module Org.Exporters.Processing.GatherAnchors (gatherAnchors) where

import Control.Category.Natural (type (~>) (..))
import Control.Category.RecursionSchemes qualified as R
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Ix.RecursionSchemes (Fix (..))
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

initialResolveState :: ResolveState
initialResolveState =
  ResolveState
    { targetDescriptionCtx = mempty
    , srcLineNumber = 0
    , knownIds = mempty
    , idGen = [show i | (i :: Int) <- [1 ..]]
    }

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
resolveSection :: OrgF (GatherT Org) SecIx -> GatherM (OrgF Org SecIx)
resolveSection (OrgSection p a s) = do
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

resolveKws :: Keywords (GatherT Org ObjIx) -> GatherM (Keywords OrgObjects)
resolveKws k = do
  k' <- mapM (mapM getCompose) k
  whenJust (do ValueKeyword n <- k' Map.!? "name"; pure n) \name -> do
    let hint = fromMaybe mempty do ParsedKeyword c <- k' Map.!? "caption"; pure c
    uid <- getUniqueId (Named name)
    registerAnchorTarget (Named name) uid hint
  return k'

gatherAnchors :: Org ix -> M (Org ix)
gatherAnchors = (`evalStateT` initialResolveState) . getCompose . (R.fold (NT go) #)
  where
    go :: ComposeIx [] OrgF (GatherT Org) ix -> GatherT Org ix
    go (ComposeIx x) =
      Compose $ Fix <$> coerce @(GatherM [_]) do
        forM x \case
          s@OrgSection' {} -> resolveSection s
          OrgElement' p a k d -> do
            k' <- resolveKws k
            OrgElement p a k' <$> case d of
              PlainList t i -> PlainList t <$> resolveListItems i
              FootnoteDef label desc -> do
                desc' <- getCompose desc
                anchor <- getUniqueId (Footnote label)
                registerFootnote label anchor (Right desc')
                return $ FootnoteDef label desc'
              Keyword key val -> do
                val' <- mapM getCompose val
                registerKeyword key val'
                return $ Keyword key val'
              y -> getCompose $ isequenceA y
          OrgObject' p a d ->
            OrgObject p a <$> do
              case d of
                FootnoteRef (FootnoteRefDef label def) -> do
                  label' <- maybe (getUniqueId (CustomId "anon")) pure label
                  anchor <- getUniqueId (Footnote label')
                  def' <- getCompose def
                  registerFootnote label' anchor (Left def')
                  return $ FootnoteRef $ FootnoteRefLabel label'
                Target name -> do
                  anchor <- getUniqueId (Named name)
                  desc <- gets (.targetDescriptionCtx)
                  registerAnchorTarget (Named name) anchor (fromMaybe mempty desc)
                  getCompose $ isequenceA d
                y -> getCompose $ isequenceA y
          y -> getCompose $ isequenceA y
