module Org.Exporters.Processing
  ( module Org.Exporters.Processing,
    module Org.Exporters.Processing.OrgData,
    module Org.Exporters.Processing.InternalLinks,
    module Org.Exporters.Processing.Prune,
    module Org.Exporters.Processing.SpecialStrings,
    module Org.Exporters.Processing.GatherKeywords,
  )
where

import Org.Exporters.Processing.GatherKeywords
import Org.Exporters.Processing.InternalLinks
import Org.Exporters.Processing.OrgData
import Org.Exporters.Processing.Prune
import Org.Exporters.Processing.SpecialStrings

withCurrentData :: F t -> M t
withCurrentData x = do
  cdata <- fix . runReader <$> gets snd
  pure $ runReader x cdata

runPipeline :: M (F t) -> (t, OrgData)
runPipeline = continuePipeline initialOrgData

continuePipeline :: OrgData -> M (F t) -> (t, OrgData)
continuePipeline data' x = (runReader resolved datum, datum)
  where
    (resolved, (_, fix . runReader -> datum)) =
      -- magic
      runState x (initialResolveState, pure data')
