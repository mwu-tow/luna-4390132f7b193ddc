{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module ZMQ.Bus.Server where

import           Control.Monad             (forever)
import           Control.Monad.Morph
import           Control.Monad.Trans
import           Control.Monad.Trans.State

import           Prologue                  hiding (error, liftIO)
import           System.Log.MLogger
import           ZMQ.Bus.Bus               (Bus)
import qualified ZMQ.Bus.Bus               as Bus
import qualified ZMQ.Bus.Data.Flag         as Flag
import           ZMQ.Bus.Data.Message      (CorrelationID, Message)
import qualified ZMQ.Bus.Data.Message      as Message
import           ZMQ.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic        (Topic)
import           ZMQ.Bus.EndPoint          (BusEndPoints)
import           ZMQ.Bus.Trans             (BusT (BusT))
import qualified ZMQ.Bus.Trans             as BusT



logger :: Logger
logger = getLogger $moduleName


run :: BusEndPoints -> [Topic] -> (Message -> IO [Message]) -> IO (Either Bus.Error ())
run endPoints topics process = Bus.runBus endPoints $ handleLoop topics process


handleLoop :: [Topic] -> (Message -> IO [Message]) -> Bus ()
handleLoop topics process = do
    mapM_ Bus.subscribe topics
    void $ forever $ handle process


handle :: (Message -> IO [Message]) -> Bus ()
handle process = do
    MessageFrame msg crlID _ _ <- Bus.receive
    liftIO $ logger debug $ "Received request: " <> (msg ^. Message.topic)
    response <- liftIO $ process msg
    unless (null response) $ do
        mapM_ (Bus.reply crlID Flag.Disable) (unsafeInit response)
        Bus.reply crlID Flag.Enable $ unsafeLast response


runState :: Bus () -> BusEndPoints -> [Topic] -> s -> (CorrelationID -> Message -> StateT s IO [Message]) -> IO (Either Bus.Error ())
runState initialize endPoints topics s process = Bus.runBus endPoints $ initialize >> handleLoopState topics s process


handleLoopState :: [Topic] -> s -> (CorrelationID -> Message -> StateT s IO [Message]) -> Bus ()
handleLoopState topics s process = do
    mapM_ Bus.subscribe topics
    void $ BusT.runBusT $ runStateT (forever $ handleState process) s


handleState :: (CorrelationID -> Message -> StateT s IO [Message]) -> StateT s BusT ()
handleState process = do
    MessageFrame msg crlID _ _ <- lift $ BusT Bus.receive
    liftIO $ logger debug $ "Received request: " <> (msg ^. Message.topic)
    response <- hoist liftIO $ process crlID msg
    lift $ BusT $ unless (null response) $ do
        mapM_ (Bus.reply crlID Flag.Disable) (unsafeInit response)
        Bus.reply crlID Flag.Enable $ unsafeLast response
