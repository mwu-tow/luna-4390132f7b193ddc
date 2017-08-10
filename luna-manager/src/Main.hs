{-# LANGUAGE CPP #-}

module Main where

import Prologue hiding (FilePath)
import Control.Monad.Raise
import Luna.Manager.Clean
import Luna.Manager.Command.Options (evalOptionsParserT)
import Luna.Manager.Command         (chooseCommand)


import Control.Concurrent (myThreadId)
import qualified Control.Exception.Safe as Exception




main :: IO ()
main = do
    tmp <- evalGetTmp
    threadId <- myThreadId
    handleSignal threadId
    evalOptionsParserT chooseCommand `Exception.finally` (cleanUp tmp)

instance Exception e => MonadException e IO where raise = Exception.throwM

handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e
