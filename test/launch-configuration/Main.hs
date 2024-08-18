module Main (main) where

import qualified LaunchConfigurationGolden (golden)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain . testGroup "launch-configuration-library"
    =<< sequence
      [ testGroup "golden" <$> LaunchConfigurationGolden.golden
      ]
