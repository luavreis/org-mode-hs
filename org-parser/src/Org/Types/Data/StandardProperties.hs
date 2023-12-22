module Org.Types.Data.StandardProperties where

data StandardProperties = StandardProperties
  { begin :: !Int
  , end :: !Int
  , postBlank :: !Int
  }
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)
