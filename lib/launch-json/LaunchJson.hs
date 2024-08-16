{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LaunchJson (main) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT)
import qualified LaunchJson.Defaults (determine)
import qualified LaunchJson.Environment (T)
import qualified LaunchJson.Options (parserInfo)
import Options.Applicative (execParser)

newtype LaunchJson a = LaunchJson
  { run :: LoggingT (ReaderT LaunchJson.Environment.T IO) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadLogger, MonadReader LaunchJson.Environment.T, MonadThrow)

runLaunchJson :: LaunchJson a -> LaunchJson.Environment.T -> IO a
runLaunchJson launchJson = runReaderT (runStderrLoggingT $ run launchJson)

main :: IO ()
main = LaunchJson.Defaults.determine >>= execParser . LaunchJson.Options.parserInfo >>= runLaunchJson main'

main' :: LaunchJson ()
main' = undefined

-- Read Cabal file
-- Generate launch.json
-- Write launch.json to .vscode/launch.json
