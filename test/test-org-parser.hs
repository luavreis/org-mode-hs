-- |

module Main
 ( module Main
 , module Tests.Helpers
 , module Org.Parser.Document
 , module Org.Parser.Elements
 , module Org.Parser.Objects
 , module Org.Exporters.Heist
 ) where

import Test.Tasty
import Tests.Helpers
import Tests.Document
import Tests.Objects
import Org.Parser.Document
import Org.Parser.Elements
import Org.Parser.Objects
import Org.Exporters.Heist

tests :: TestTree
tests = testGroup "Org parser tests"
        [ testObjects
        , testDocument
        ]

main :: IO ()
main = do
  defaultMain tests
