module Org.Exporters.Processing.SpecialStrings (doSpecialStrings, processSpecialStrings) where

import Data.Text qualified as T
import Org.Types.Variants.Annotated
import qualified Data.Ix.RecursionSchemes as R
import Control.Category.Natural (type (~>) (..))

doSpecialStrings :: Text -> Text
doSpecialStrings txt =
  txt
    & T.replace "---" "—"
    & T.replace "--" "–"
    & T.replace "..." "…"
    & T.replace "\\-" "\173"
    & T.replace "'" "’"

processSpecialStrings :: Org ix -> Org ix
processSpecialStrings = (#) $ R.fold $ NT \(ComposeIx x) -> coerce $ map process x
  where
    process (OrgObject' p a (Plain txt)) = OrgObject p a $ Plain $ doSpecialStrings txt
    process x = x
