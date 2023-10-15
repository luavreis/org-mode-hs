module Org.Types.StandardProperties where
import Data.Data (Data)

data Pos = Pos
  { begin :: !Int
  , end :: !Int
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
  deriving anyclass (NFData)
