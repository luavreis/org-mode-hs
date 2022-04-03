-- |

module Tests.Objects where
import Tests.Helpers
import Org.Parser.Objects
import Org.Types
import qualified Org.Builder as B

testObjects :: TestTree
testObjects = testGroup "Objects"
  [ "Timestamp" ~: timestamp $
    [
      "<1997-11-03 Mon 19:15>" =?>
      B.timestamp
        (TimestampData True ((1997,11,3, Just "Mon"), Just (19,15), Nothing, Nothing))
    ]

  , "Citation with corner case markups" ~: citation $
    [
      "[cite:/foo/;/bar/@bef=bof=;/baz/]" =?>
      let ref = CiteReference
            { refId = "bef"
            , refPrefix = [ Italic [ Plain "bar" ] ]
            , refSuffix = [ Verbatim "bof" ]
            }
      in B.citation
        Citation
          { citationStyle = ""
          , citationVariant = ""
          , citationPrefix = [ Italic [ Plain "foo" ] ]
          , citationSuffix = [ Italic [ Plain "baz" ] ]
          , citationReferences = [ref]
          }
    ]
  ]
