module Org.Exporters.Processing.Prune where

import Data.Set qualified as Set
import Org.Exporters.Processing.OrgData
import Org.Parser.Definitions

-- | Prunes COMMENT, :ARCHIVE: and noexport-tagged sections
pruneSections :: Set Text -> Set Text -> [OrgSection] -> ([OrgSection], Bool)
pruneSections selTags excTags = prune . mapMaybe go
  where
    prune ss =
      if any snd ss
        then (map fst $ filter snd ss, True)
        else (map fst ss, False)
    go s =
      if sectionIsComment s || not (null $ Set.intersection tagSet excTags) || Set.member "archive" tagSet
        then Nothing
        else
          let (ss, e) = pruneSections selTags excTags (sectionSubsections s)
              e' = not (null $ Set.intersection tagSet selTags)
              s' = s {sectionSubsections = ss}
           in Just (s', e' || e)
      where
        tagSet :: Set Text = fromList $ sectionTags s

-- | Prunes COMMENT, :ARCHIVE: and noexport-tagged sections
pruneDoc :: OrgDocument -> F OrgDocument
pruneDoc doc = do
  selTags <- asks (fromList . orgExportSelectTags . exporterSettings)
  excTags <- asks (fromList . orgExportExcludeTags . exporterSettings)
  let (ss, e) = pruneSections selTags excTags (documentSections doc)
  pure
    doc
      { documentChildren = bool (documentChildren doc) [] e,
        documentSections = ss
      }
