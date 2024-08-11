module LaunchJsonSpec (spec) where

import qualified LaunchJsonSpec.OptionsSpec (spec)
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "LaunchJson" $ do
  LaunchJsonSpec.OptionsSpec.spec
