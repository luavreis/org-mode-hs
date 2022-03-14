-- |

module Tests.Objects where
import Tests.Helpers
import Text.Org.Parser.OrgObjects

tests :: [TestTree]
tests =
  [ "Timestamp 1" ~: timestamp $
    "<1997-11-03 Mon 19:15>" =?>
    [ ]
  ]
