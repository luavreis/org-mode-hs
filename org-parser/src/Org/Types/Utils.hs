module Org.Types.Utils where

import Data.Ix.Functor (IFunctor)
import Org.Types.Data.Document (OrgDocumentData (..))
import Org.Types.Data.Section (OrgSectionData (..))
import Org.Types.Variants.Plain (Org, OrgF, OrgIx (..), mapIx, secData)
import Org.Types.Walk (walk)
import Relude.Extra.Lens (over)

-- * Sections

isolateSection ::
  (IFunctor f) =>
  OrgSectionData (Org f) SecIx ->
  OrgDocumentData (Org f) ix
isolateSection section =
  OrgDocumentData
    { properties = section.properties
    , children = section.children
    , sections = walk (mapIx id (shiftSection (-section.level))) section.subsections
    }

shiftSection :: Int -> OrgF k SecIx -> OrgF k SecIx
shiftSection i =
  over secData \sec@(OrgSectionData {level = level}) ->
    if level + i > 0
      then sec {level = level + i}
      else sec {level = 1}
