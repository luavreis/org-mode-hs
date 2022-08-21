module Tests.Document where

import NeatInterpolation
import Org.Parser.Document (propertyDrawer)
import Tests.Helpers

testDocument :: TestTree
testDocument =
  testGroup
    "Document"
    [ "Property drawer" ~: propertyDrawer $
        [ [text|   :pRoPerTieS:
            :Fo^o3': 	 bar
              :foobar:
            :fooBARbar: bla bla
             :ENd:
      |]
            =?> fromList
              [ ("fo^o3'", "bar"),
                ("foobar", ""),
                ("foobarbar", "bla bla")
              ],
          [text|:properties:
            :end:
      |]
            =?> mempty
        ]
    ]
