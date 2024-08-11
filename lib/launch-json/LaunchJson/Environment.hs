module LaunchJson.Environment (T (..)) where

import System.OsPath (OsPath)

data T = T
  { cabal :: OsPath,
    vscode :: OsPath
  }
  deriving (Show)
