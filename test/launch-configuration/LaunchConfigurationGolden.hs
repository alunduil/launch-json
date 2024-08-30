module LaunchConfigurationGolden (golden) where

import Data.Yaml (encode)
import qualified LaunchConfiguration as SUT
import System.FilePath (takeBaseName, (-<.>))
import System.OsPath (encodeFS)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

golden :: IO [TestTree]
golden =
  sequence
    [ testGroup "encode <$> LaunchConfiguration.fromPath"
        . map
          ( \p ->
              goldenVsString
                (takeBaseName p)
                (p -<.> ".yaml")
                (encode <$> (SUT.fromPath =<< encodeFS p))
          )
        <$> findByExtension [".cabal"] "test/launch-configuration/data"
    ]
