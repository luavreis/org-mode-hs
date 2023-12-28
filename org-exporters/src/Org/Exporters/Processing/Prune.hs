module Org.Exporters.Processing.Prune where

import Control.Category.Natural (type (~>) (..))
import Control.Monad.Trans.Writer.CPS
import Data.Ix.RecursionSchemes qualified as R
import Data.Ix.Traversable (isequenceA)
import Data.Set qualified as Set
import Org.Exporters.Processing.OrgData
import Org.Types.Variants.Annotated

newtype Selected = Selected Any
  deriving newtype (Semigroup, Monoid)

type PruneM = Writer Selected
type PruneT = Compose PruneM

markAsSelected :: PruneM ()
markAsSelected = tell (Selected (Any True))

-- | Prunes COMMENT, :ARCHIVE: and noexport-tagged sections
pruneSection :: Set Text -> Set Text -> OrgSectionData (PruneT k) ix -> Maybe (PruneT (OrgSectionData k) ix)
pruneSection selTags excTags section =
  if not section.comment && null (Set.intersection tagSet excTags) && Set.notMember "archive" tagSet
    then Just $ Compose do
      unless (null $ Set.intersection tagSet selTags) markAsSelected
      coerce $ isequenceA section
    else Nothing
  where
    tagSet = fromList section.tags

-- | Prunes COMMENT, :ARCHIVE: and noexport-tagged sections
pruneSections :: Set Text -> Set Text -> ComposeIx [] OrgF (PruneT k) ix -> PruneT (ComposeIx [] OrgF k) ix
pruneSections selTags excTags (coerce -> input) = Compose $ fmap ComposeIx do
  (sections', someSelected) <- listen $ sequence sections
  if coerce someSelected
    then mapMaybeM pruneSel sections
    else return sections'
  where
    sections = mapMaybe prune input
    prune :: OrgF (PruneT k) ix -> Maybe (PruneM (OrgF k ix))
    prune = \case
      OrgSection' p a d -> do
        res <- coerce $ pruneSection selTags excTags d
        return $ OrgSection p a <$> res
      x -> Just $ coerce $ isequenceA x
    pruneSel x = do
      (x', selected) <- listen x
      if coerce selected
        then return (Just x')
        else return Nothing

-- | Prunes COMMENT, :ARCHIVE: and noexport-tagged sections
pruneDoc :: OrgDocument -> M OrgDocument
pruneDoc doc = do
  selTags <- gets \d -> d.exporterSettings.orgExportSelectTags
  excTags <- gets \d -> d.exporterSettings.orgExportExcludeTags
  let (prunedSections, coerce -> someSelected) =
        runWriter $ coerce $ R.transverse (NT $ pruneSections selTags excTags) # doc.sections
  return
    $ doc
      { children = if someSelected then doc.children else mempty
      , sections = prunedSections
      }
