{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LaunchJson (main) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logInfo, runStderrLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, liftIO, runReaderT)
import Data.Aeson (encode)
import Data.ByteString.Lazy (putStr)
import Data.Text (pack)
import qualified LaunchConfiguration (fromPath)
import qualified LaunchJson.Defaults (determine)
import LaunchJson.Environment (T (cabal))
import qualified LaunchJson.Options (parserInfo)
import Options.Applicative (execParser)
import Prelude hiding (putStr)

newtype LaunchJson a = LaunchJson
  { run :: LoggingT (ReaderT LaunchJson.Environment.T IO) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadLogger, MonadReader LaunchJson.Environment.T, MonadThrow)

runLaunchJson :: LaunchJson a -> LaunchJson.Environment.T -> IO a
runLaunchJson launchJson = runReaderT (runStderrLoggingT $ run launchJson)

main :: IO ()
main = LaunchJson.Defaults.determine >>= execParser . LaunchJson.Options.parserInfo >>= runLaunchJson main'

main' :: LaunchJson ()
main' = do
  input <- asks cabal
  $logInfo $ pack $ "processing: " ++ show input
  launchConfiguration <- LaunchConfiguration.fromPath input
  $logDebug $ pack $ "launchConfiguration: " ++ show launchConfiguration
  liftIO $ putStr $ encode launchConfiguration
