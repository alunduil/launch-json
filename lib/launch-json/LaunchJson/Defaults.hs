{-# LANGUAGE RecordWildCards #-}

module LaunchJson.Defaults (T (..), determine) where

import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (listToMaybe)
import System.FilePath.Glob (compile, globDir1)
import System.OsPath (OsPath, decodeFS, encodeFS, encodeUtf)
import System.Process (readProcess)

data T = T
  { dCabal :: Maybe OsPath,
    dVisualStudioCode :: Maybe OsPath
  }

determine :: IO T
determine = do
  root' <- root >>= decodeFS
  dCabal <- find "*.cabal" root'
  dVisualStudioCode <- find ".vscode" root'
  return T {..}
  where
    find glob path = (encodeUtf <=< listToMaybe) <$> globDir1 (compile glob) path

root :: IO OsPath
root = do
  output <- nonEmpty . lines <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
  case output of
    Just (x :| _) -> encodeFS x
    Nothing -> fail "`git rev-parse --show-toplevel` returned no output"
