{-# LANGUAGE RecordWildCards #-}

module Org.Exporters.Processing
  ( module Org.Exporters.Processing
  , module Org.Exporters.Processing.OrgData
  , module Org.Exporters.Processing.GatherSettings
  , module Org.Exporters.Processing.Prune
  , module Org.Exporters.Processing.GatherAnchors
  , module Org.Exporters.Processing.ResolveLinks
  , module Org.Exporters.Processing.SpecialStrings
  )
where

import Org.Exporters.Processing.GatherAnchors
import Org.Exporters.Processing.GatherSettings
import Org.Exporters.Processing.OrgData
import Org.Exporters.Processing.Prune
import Org.Exporters.Processing.ResolveLinks
import Org.Exporters.Processing.SpecialStrings

import Control.Category (Category (..))
import Control.Category.Natural (type (~>) (..))
import Control.Category.RecursionSchemes (Corecursive (embed))
import Control.Category.RecursionSchemes qualified as R
import Org.Types.Ix (ComposeIx (..))
import Org.Types.Variants.Annotated qualified as A
import Org.Types.Variants.ParseInfo qualified as P
import Prelude hiding ((.))
import Data.Ix.Functor (ifmap)
import Data.Ix.Traversable (itraverse)

convert :: P.Org ix -> A.Org ix
convert = (R.fold (embed . NT (coerce f)) #)
  where
    f :: Seq (P.OrgF A.Org ix) -> [A.OrgF A.Org ix]
    f x = g <$> toList x
    g u = ComposeIx $ Compose (A.AnnW mempty u)

processAll :: P.OrgDocumentData P.Org ix -> (A.OrgDocumentData A.Org ix, OrgData)
processAll doc =
  (`runState` initialOrgData) do
    pruned <- pruneDoc doc'
    gatherSettings pruned
    withAnchors <- itraverse gatherAnchors pruned
    withLinks <- itraverse resolveLinks withAnchors
    let withSpecialStrings = ifmap processSpecialStrings withLinks
    return withSpecialStrings
  where
    doc' = ifmap convert doc
