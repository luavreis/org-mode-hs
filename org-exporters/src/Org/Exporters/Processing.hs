{-# LANGUAGE RecordWildCards #-}
module Org.Exporters.Processing
  ( module Org.Exporters.Processing
  --   module Org.Exporters.Processing.OrgData,
  --   module Org.Exporters.Processing.InternalLinks,
  --   module Org.Exporters.Processing.Prune,
  --   module Org.Exporters.Processing.SpecialStrings,
  --   module Org.Exporters.Processing.GatherSettings,
  )
where

-- import Org.Exporters.Processing.GatherSettings
-- import Org.Exporters.Processing.InternalLinks
import Org.Exporters.Processing.OrgData
-- import Org.Exporters.Processing.Prune
-- import Org.Exporters.Processing.SpecialStrings

import Control.Category (Category (..))
import Control.Category.Natural (type (~>) (..))
import Control.Category.RecursionSchemes (Corecursive (embed))
import Control.Category.RecursionSchemes qualified as R
import Org.Types.Ix (ComposeIx (..))
import Org.Types.Variants.Annotated qualified as A
import Org.Types.Variants.ParseInfo qualified as P
import Prelude hiding ((.))

process :: P.Org ix -> A.Org ix
process = (R.fold (embed . NT (coerce f)) #)
  where
    f :: Seq (P.OrgF A.Org ix) -> [A.OrgF A.Org ix]
    f x = g <$> toList x
    g (P.OrgF {..}) = A.OrgF {annotations = mempty, ..}

-- withCurrentData :: F t -> M t
-- withCurrentData x = do
--   cdata <- fix . runReader <$> gets snd
--   pure $ runReader x cdata

-- processAll :: OrgDocument -> (OrgDocument, OrgData)
-- processAll p = runPipeline do
--   gatherSettings p
--   pruned <- withCurrentData $ pruneDoc p
--   let processed = processSpecialStrings pruned
--   getCompose $ resolveLinks processed

-- runPipeline :: M (F t) -> (t, OrgData)
-- runPipeline = continuePipeline initialOrgData

-- continuePipeline :: OrgData -> M (F t) -> (t, OrgData)
-- continuePipeline data' x = (runReader resolved datum, datum)
--   where
--     (resolved, (_, fix . runReader -> datum)) =
--       runState x (initialResolveState, pure data')
