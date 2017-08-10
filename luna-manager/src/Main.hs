{-# LANGUAGE CPP #-}

module Main where

import Prologue hiding (FilePath)
import Control.Monad.Raise
import Luna.Manager.Clean
import Luna.Manager.Command.Options (evalOptionsParserT)
import Luna.Manager.Command         (chooseCommand)


import Control.Concurrent (myThreadId)
import qualified Control.Exception.Safe as Exception
import qualified Luna.Manager.Shell.Shelly as Shelly


main :: IO ()
main = run

run :: (MonadIO m, MonadException SomeException m, MonadMask m) => m () -- MonadException SomeException m -- wywalic!
run = Shelly.shelly $ do
    tmp <- evalGetTmp
    threadId <- liftIO myThreadId
    liftIO $ handleSignal threadId
    evalOptionsParserT chooseCommand -- `Exception.finally` (cleanUp tmp)

handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e
