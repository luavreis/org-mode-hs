module Org.Types.Data.Timestamp
  ( TimestampData (..)
  , OrgDateTime
  , TimestampMark
  , OrgDate (..)
  , OrgTime (..)
  ) where

-- | An Org timestamp, including repetition marks.
data TimestampData
  = TimestampData {active :: Bool, time :: OrgDateTime}
  | TimestampRange {active :: Bool, start :: OrgDateTime, end :: OrgDateTime}
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

data OrgDate = OrgDate {year :: Int, month :: Int, day :: Int, weekday :: Maybe Text}
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

data OrgTime = OrgTime {hour :: Int, minute :: Int}
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
  deriving anyclass (NFData)

type TimestampMark = (Text, Int, Char)

type OrgDateTime = (OrgDate, Maybe OrgTime, Maybe TimestampMark, Maybe TimestampMark)
