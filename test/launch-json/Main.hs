module Main (main) where

import qualified LaunchJsonSpec (spec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  specs <- testSpec "hspec" LaunchJsonSpec.spec
  defaultMain $
    testGroup
      "launch-json-library"
      [ specs
      ]
