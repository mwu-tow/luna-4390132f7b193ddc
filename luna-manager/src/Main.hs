module Main where

import Prologue hiding (FilePath)
import Control.Monad.Raise
import Luna.Manager.Command.Options (evalOptionsParserT)
import Luna.Manager.Command         (chooseCommand)

import Filesystem.Path.CurrentOS (encodeString, decodeString, FilePath)
import System.Directory (removeDirectoryRecursive)
import Luna.Manager.System.Env (getTmpPath, EnvConfig)
import Luna.Manager.System.Host
import Control.Monad.Raise
import Control.Monad.State.Layered
import GHC.ConsoleHandler
import Control.Concurrent (threadDelay, ThreadId, killThread, myThreadId)
import System.Process.Typed
import qualified Control.Exception.Safe as Exception
import qualified System.Signal as Signal

evalGetTmp :: (MonadIO m) => m FilePath
evalGetTmp = evalDefHostConfigs @'[EnvConfig] $ getTmpPath

cleanUp :: MonadIO m => FilePath -> m ()
cleanUp tmp = do
    -- runProcess_ $ shell ("RD /S /Q " ++ (encodeString tmp))
    liftIO $ removeDirectoryRecursive $ encodeString tmp

termHandler :: ThreadId -> Signal.Signal -> IO ()
termHandler tId s = do
    killThread tId

termHandler2 :: ThreadId -> Handler
termHandler2 tid  = Catch $ \_ -> do
    killThread tid

main :: IO ()
main = do
    tmp <- evalGetTmp
    threadId <- myThreadId
    case currentHost of
        Windows -> void $ installHandler (termHandler2 threadId )
        Linux -> do
            Signal.installHandler Signal.sigTERM (termHandler threadId)
            Signal.installHandler Signal.sigINT (termHandler threadId)
        Darwin -> do
            Signal.installHandler Signal.sigTERM (termHandler threadId)
            Signal.installHandler Signal.sigINT (termHandler threadId)
    evalOptionsParserT chooseCommand `Exception.finally` (cleanUp tmp)

instance Exception e => MonadException e IO where raise = Exception.throwM

handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e
