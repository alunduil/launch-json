{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module LaunchJson.Options (parserInfo) where

import Control.Applicative ((<**>))
import Control.Monad ((>=>))
import LaunchJson.Defaults (T (dVisualStudioCode))
import qualified LaunchJson.Defaults (T (..))
import qualified LaunchJson.Environment (T (..))
import Options.Applicative
  ( HasValue,
    Mod,
    Parser,
    ParserInfo,
    ReadM,
    fullDesc,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    progDesc,
    showDefault,
    value,
  )
import System.OsPath (OsPath, encodeUtf, isValid)

parserInfo :: LaunchJson.Defaults.T -> Options.Applicative.ParserInfo LaunchJson.Environment.T
parserInfo ds =
  info
    (parser ds <**> helper)
    ( fullDesc
        <> progDesc
          ( unlines
              [ "Create launch.json in VSCODE for CABAL"
              ]
          )
    )

parser :: LaunchJson.Defaults.T -> Options.Applicative.Parser LaunchJson.Environment.T
parser LaunchJson.Defaults.T {..} = do
  cabal <-
    option
      path
      ( long "cabal-path"
          <> help "Path to cabal file used as source for launch.json."
          <> metavar "CABAL"
          <> maybeDefault dCabal
      )
  vscode <-
    option
      path
      ( long "vscode-path"
          <> help "Path to vscode directory used as target for launch.json."
          <> metavar "VSCODE"
          <> maybeDefault dVisualStudioCode
      )
  pure LaunchJson.Environment.T {..}

path :: Options.Applicative.ReadM System.OsPath.OsPath
path = maybeReader (encodeUtf >=> \p -> if isValid p then Just p else Nothing)

maybeDefault :: (HasValue f, Show a) => Maybe a -> Mod f a
maybeDefault (Just a) = value a <> showDefault
maybeDefault Nothing = mempty
