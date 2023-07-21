module Org.Exporters.Processing
  ( module Org.Exporters.Processing,
    module Org.Exporters.Processing.OrgData,
    module Org.Exporters.Processing.InternalLinks,
    module Org.Exporters.Processing.Prune,
    module Org.Exporters.Processing.SpecialStrings,
    module Org.Exporters.Processing.GatherSettings,
  )
where

import Org.Exporters.Processing.GatherSettings
import Org.Exporters.Processing.InternalLinks
import Org.Exporters.Processing.OrgData
import Org.Exporters.Processing.Prune
import Org.Exporters.Processing.SpecialStrings
import Org.Types (OrgDocument)

withCurrentData :: F t -> M t
withCurrentData x = do
  cdata <- fix . runReader <$> gets snd
  pure $ runReader x cdata

processAll :: OrgDocument -> (OrgDocument, OrgData)
processAll p = runPipeline do
  gatherSettings p
  pruned <- withCurrentData $ pruneDoc p
  let processed = processSpecialStrings pruned
  getCompose $ resolveLinks processed

runPipeline :: M (F t) -> (t, OrgData)
runPipeline = continuePipeline initialOrgData

continuePipeline :: OrgData -> M (F t) -> (t, OrgData)
continuePipeline data' x = (runReader resolved datum, datum)
  where
    (resolved, (_, fix . runReader -> datum)) =
      runState x (initialResolveState, pure data')
