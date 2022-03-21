-- |

module Main
 ( module Main
 , module Tests.Helpers
 ) where

import Test.Tasty
import Tests.Helpers
import qualified Tests.Elements as Elements
import Text.Org.Parser.OrgElements (para)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Org parser tests"
        [ testGroup "Elements" Elements.tests
        ]

main :: IO ()
main = do
  getArgs >>= \case
    x:_ -> do
      print (prettyParse para $ T.replicate 10000 (T.pack x))
    _ -> pure ()
  -- defaultMain tests
