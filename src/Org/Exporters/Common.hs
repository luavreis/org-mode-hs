-- |

module Org.Exporters.Common
  ( ExporterSettings (..)
  , defaultExporterSettings
  , doSpecialStrings
  , processSpecialStrings
  ) where
import Org.Types
import Org.Walk
import Data.Time (TimeLocale, defaultTimeLocale)
import Data.Text qualified as T

data ExporterSettings = ExporterSettings
  { orgExportHeadlineLevels :: Int
  , orgExportWithSpecialStrings :: Bool
  , headlineLevelShift :: Int
  , timeLocale :: TimeLocale
  } deriving (Eq, Show)

defaultExporterSettings :: ExporterSettings
defaultExporterSettings = ExporterSettings
  { orgExportHeadlineLevels = 3
  , orgExportWithSpecialStrings = True
  , headlineLevelShift = 0
  , timeLocale = defaultTimeLocale
  }

doSpecialStrings :: Text -> Text
doSpecialStrings txt =
  txt & T.replace "---" "—"
      & T.replace "--" "–"
      & T.replace "..." "…"
      & T.replace "\\-" "\173"

processSpecialStrings :: Walkable OrgInline a => a -> a
processSpecialStrings = walk process
  where
    process :: OrgInline -> OrgInline
    process (Plain txt) = Plain $ doSpecialStrings txt
    process x = x
