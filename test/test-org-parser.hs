-- |

module Main
 ( module Main
 , module Tests.Helpers
 ) where

import Test.Tasty
import Tests.Helpers
import qualified Tests.Elements as Elements

tests :: TestTree
tests = testGroup "Org parser tests"
        [ testGroup "Elements" Elements.tests
        ]

main :: IO ()
main = do
  defaultMain tests
