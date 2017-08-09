module Main where

import Prologue hiding (FilePath)
import Control.Monad.Raise
import Luna.Manager.Command.Options (evalOptionsParserT)
import Luna.Manager.Command         (chooseCommand)

import Filesystem.Path.CurrentOS (encodeString, FilePath)
import System.Directory (removeDirectoryRecursive)
import Luna.Manager.System.Env (getTmpPath, EnvConfig)
import Luna.Manager.System.Host
import Control.Monad.Raise
import Control.Monad.State.Layered
import System.Signal
import Control.Concurrent (threadDelay, ThreadId, killThread, myThreadId)


evalGetTmp :: (MonadIO m) => m FilePath
evalGetTmp = evalDefHostConfigs @'[EnvConfig] $ getTmpPath

cleanUp :: MonadIO m => FilePath -> m ()
cleanUp tmp = liftIO $ removeDirectoryRecursive $ encodeString tmp

termHandler :: FilePath -> ThreadId -> Signal -> IO ()
termHandler tmp tId s = do
    putStrLn $ "Exit with code: " <> (show s)
    cleanUp tmp
    killThread tId

main :: IO ()
main = do
    tmp <- evalGetTmp
    threadId <- myThreadId
    installHandler sigTERM (termHandler tmp threadId)
    installHandler sigINT (termHandler tmp threadId)
    handleAll handleTopLvlError $ evalOptionsParserT chooseCommand

handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e
