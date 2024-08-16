{-# LANGUAGE OverloadedStrings #-}

module LaunchJsonSpec.OptionsSpec (spec) where

import Data.Text (Text, isInfixOf, pack)
import LaunchJson.Defaults (T (..))
import qualified LaunchJson.Environment (T)
import qualified LaunchJson.Options as SUT
import Options.Applicative
  ( ParserResult (CompletionInvoked, Failure, Success),
    defaultPrefs,
    execParserPure,
    renderFailure,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.OsPath (encodeUtf)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "Options" $ do
  describe "parserInfo" $ do
    it "should error if --cabal-path is \"\"" $
      parse ["--cabal-path", ""]
        `shouldSatisfy` failWith
          "option --cabal-path: cannot parse value `'"
    it "should error if --vscode-path is \"\"" $
      parse ["--vscode-path", ""]
        `shouldSatisfy` failWith
          "option --vscode-path: cannot parse value `'"

failWith :: Text -> ParserResult a -> Bool
failWith _ (Success _) = False
failWith _ (CompletionInvoked _) = False
failWith m (Failure f) =
  case renderFailure f "" of
    (_, ExitSuccess) -> False
    (m', ExitFailure _) -> m `isInfixOf` pack m'

parse :: [String] -> ParserResult LaunchJson.Environment.T
parse = execParserPure defaultPrefs (SUT.parserInfo defaults)

defaults :: LaunchJson.Defaults.T
defaults =
  LaunchJson.Defaults.T
    { dCabal = encodeUtf "default",
      dVisualStudioCode = encodeUtf "default"
    }
