-- |

module Tests.Elements where
import Tests.Helpers
import Text.Org.Parser.OrgDocument (propertyDrawer)

tests :: [TestTree]
tests =
  [ "Property drawer" ~: propertyDrawer $
    unlines [ "  :pRoPerTieS:    "
            , ":Fo^o3': \t bar  "
            , ":foobar:"
            , ":foobarbar: bla bla "
            , " :ENd:"
            ] =?>
    [ ("Fo^o3'", "bar")
    , ("foobar", "")
    , ("foobarbar", "bla bla")
    ]
  ]

