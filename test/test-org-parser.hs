-- |

module Main
 ( module Main
 , module Tests.Helpers
 ) where

import Test.Tasty
import Tests.Helpers
import Tests.Document
import Tests.Objects

tests :: TestTree
tests = testGroup "Org parser tests"
        [ testObjects
        , testDocument
        ]

main :: IO ()
main = do
  defaultMain tests
