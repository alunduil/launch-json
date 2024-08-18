{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LaunchConfiguration (LaunchConfiguration, fromPath) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.OsPath (OsPath, decodeUtf, encodeFS)

-- |
--  launch.json configuration for Haskell
--
--  = References
--
--  - [Visual Studio Code Marketplace Haskell GHCi Debug Adapter Phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
--  - [Further examples](https://github.com/phoityne/hdx4vsc/tree/master/configs)
data LaunchConfiguration = LaunchConfiguration
  { -- | type of launch configuration; should be "ghc"
    --   Example: "ghc"
    configType :: LaunchConfigurationType,
    -- | type of request; should be "launch"
    --   Example: "launch"
    request :: LaunchConfigurationRequest,
    -- | name of the configuration; appears in the drop-down list on the Debug viewlet
    --   Example: "haskell(cabal)"
    name :: Text,
    -- | internal console options; controls the visibility of the internal debug console
    --   Example: "openOnSessionStart"
    internalConsoleOptions :: LaunchConfigurationInternalConsoleOptions,
    -- | workspace folder
    --   Example: "${workspaceFolder}"
    workspace :: OsPath,
    -- | debug startup file which will be loaded automatically
    --   Example: "${workspaceFolder}/test/Spec.hs"
    startup :: OsPath,
    -- | debug startup function which will be run instead of main
    --   Example: "main"
    startupFunc :: Text,
    -- | arguments for 'startupFunc' as a single string
    --   Example: "arg1 arg2"
    startupArgs :: Text,
    -- | stop or not after the debugger is launched
    --   Example: False
    stopOnEntry :: Bool,
    -- | main arguments as a single string
    --   Example: "arg1 arg2"
    mainArgs :: Text,
    -- | GHCi command prompt
    --   Example: "H>>= "
    ghciPrompt :: Text,
    -- | GHCi initial prompt; use this if you set a custom prompt in ~/.ghci
    --   Example: "Prelude> "
    ghciInitialPrompt :: Maybe Text,
    -- | GHCi command to start the REPL
    --  Example: "cabal repl --with-compiler ghci-dap --repl-no-load --builddir=${workspaceRoot}/.vscode/dist-cabal-repl launch-json-test"
    ghciCmd :: Text,
    -- | GHCi environment variables
    --   Example: {}
    ghciEnv :: Map Text Text,
    -- | log file path
    --   Example: "${workspaceRoot}/.vscode/phoityne.log"
    logFile :: OsPath,
    -- | log level
    --   Example: "WARNING"
    logLevel :: Text,
    -- | force inspect scope variables
    --   Example: False
    forceInspect :: Bool
  }
  deriving (Show)

--  = Example JSON configuration
--
--  {
--    "type": "ghc",
--    "request": "launch",
--    "name": "haskell(cabal)",
--    "internalConsoleOptions": "openOnSessionStart",
--    "workspace": "${workspaceFolder}",
--    "startup": "${workspaceFolder}/test/launch-json/Main.hs",
--    "startupFunc": "",
--    "startupArgs": "",
--    "stopOnEntry": false,
--    "mainArgs": "",
--    "ghciPrompt": "H>>= ",
--    "ghciInitialPrompt": "> ",
--    "ghciCmd": "cabal repl --with-compiler ghci-dap --repl-no-load --builddir=${workspaceFolder}/.vscode/dist-cabal-repl launch-json-test",
--    "ghciEnv": {},
--    "logFile": "${workspaceFolder}/.vscode/launch-json-test.log",
--    "logLevel": "DEBUG",
--    "forceInspect": false
--  }
instance ToJSON LaunchConfiguration where
  toJSON (LaunchConfiguration {..}) =
    object
      [ "type" .= toJSON configType,
        "request" .= request,
        "name" .= name,
        "internalConsoleOptions" .= internalConsoleOptions,
        "workspace" .= toJSON (decodeUtf workspace :: Maybe FilePath),
        "startup" .= toJSON (decodeUtf startup :: Maybe FilePath),
        "startupFunc" .= startupFunc,
        "startupArgs" .= startupArgs,
        "stopOnEntry" .= stopOnEntry,
        "mainArgs" .= mainArgs,
        "ghciPrompt" .= ghciPrompt,
        "ghciInitialPrompt" .= ghciInitialPrompt,
        "ghciCmd" .= ghciCmd,
        "ghciEnv" .= ghciEnv,
        "logFile" .= toJSON (decodeUtf logFile :: Maybe FilePath),
        "logLevel" .= logLevel,
        "forceInspect" .= forceInspect
      ]

-- Intentional singleton
data LaunchConfigurationType = GHC deriving (Show, Generic, ToJSON)

data LaunchConfigurationRequest {- TODO Attach | -} = Launch deriving (Show, Generic, ToJSON)

data LaunchConfigurationInternalConsoleOptions
  = NeverOpen
  deriving
    ( -- | OpenOnSessionStart
      -- | OpenOnFirstSessionStart
      Show,
      Generic,
      ToJSON
    )

fromPath :: (MonadIO m) => OsPath -> m [LaunchConfiguration]
fromPath _ = do
  let configType = GHC
  let request = Launch
  let name = ""
  let internalConsoleOptions = NeverOpen
  workspace <- liftIO $ encodeFS ""
  startup <- liftIO $ encodeFS ""
  let startupFunc = ""
  let startupArgs = ""
  let stopOnEntry = False
  let mainArgs = ""
  let ghciPrompt = ""
  let ghciInitialPrompt = Nothing
  let ghciCmd = ""
  let ghciEnv = mempty
  logFile <- liftIO $ encodeFS ""
  let logLevel = ""
  let forceInspect = False
  pure [LaunchConfiguration {..}]
