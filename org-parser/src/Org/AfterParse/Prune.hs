-- | Defines functions for prunning commented-out parts of the AST.
module Org.AfterParse.Prune
  ( pruneSection,
  )
where

import Org.Parser.Definitions

-- | Prunes COMMENT, :ARCHIVE: and noexport-tagged sections
pruneSection :: OrgSection -> OrgSection
