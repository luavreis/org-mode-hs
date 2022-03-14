-- |

module Main where

import Test.Tasty
import qualified Tests.Elements as Elements

tests :: TestTree
tests = testGroup "Org parser tests"
        [ testGroup "Elements" Elements.tests
        ]

main :: IO ()
main = do
  defaultMain tests
