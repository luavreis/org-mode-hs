module Tests.Document where

import Org.Parser.Document (propertyDrawer)
import Tests.Helpers

testDocument :: TestTree
testDocument =
  goldenGroup
    "document"
    [ "property drawer"
        ~: propertyDrawer
        $ [ unlines
              [ "   :pRoPerTieS:"
              , ":Fo^o3': \t bar"
              , "  :foobar:"
              , ":fooBARbar: bla bla"
              , " :ENd:"
              ]
          , unlines
              [ ":properties"
              , ":end:"
              ]
          ]
    ]
