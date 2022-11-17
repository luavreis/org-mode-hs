module Org.Exporters.Processing.SpecialStrings where

import Data.Text qualified as T
import Org.Types
import Org.Walk

doSpecialStrings :: Text -> Text
doSpecialStrings txt =
  txt
    & T.replace "---" "—"
    & T.replace "--" "–"
    & T.replace "..." "…"
    & T.replace "\\-" "\173"

processSpecialStrings :: MultiWalk MWTag a => a -> a
processSpecialStrings = walk process
  where
    process :: OrgObject -> OrgObject
    process (Plain txt) = Plain $ doSpecialStrings txt
    process x = x
