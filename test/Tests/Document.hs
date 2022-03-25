-- |

module Tests.Document where
import Tests.Helpers
import NeatInterpolation
import Text.Org.Parser.Document (propertyDrawer)

testDocument :: TestTree
testDocument = testGroup "Document"
  [ "Property drawer" ~: propertyDrawer $
    [
      [text|   :pRoPerTieS:
            :Fo^o3': 	 bar
              :foobar:
            :foobarbar: bla bla
             :ENd:
      |]
        =?> [ ("Fo^o3'", "bar")
            , ("foobar", "")
            , ("foobarbar", "bla bla")
            ]

    , [text|:properties:
            :end:
      |]
        =?> []
    ]
  ]
