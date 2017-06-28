{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module ZMQ.Bus.RPC.Server.Server where

import           Prologue                     hiding (error)
import           System.Log.MLogger
import           ZMQ.Bus.Bus                  (Bus)
import           ZMQ.Bus.EndPoint             (BusEndPoints)
import           ZMQ.Bus.RPC.HandlerMap       (HandlerMap)
import qualified ZMQ.Bus.RPC.HandlerMap       as HandlerMap
import qualified ZMQ.Bus.RPC.Server.Processor as Processor
import qualified ZMQ.Bus.Server               as Server
import qualified ZMQ.Bus.Util                 as Util

logger :: Logger
logger = getLogger $moduleName


run :: String -> Bus () -> BusEndPoints -> s -> HandlerMap s IO -> IO (Either String ())
run pluginName initialize endPoints s handlerMap =
    Server.runState initialize endPoints (HandlerMap.topics pluginName handlerMap) s $ Processor.process handlerMap


runWithInit :: String -> Bus () -> BusEndPoints -> s -> HandlerMap s IO -> IO (Either String ())
runWithInit pluginName initialize endPoints s handlerMap =
    Server.runState initialize endPoints (HandlerMap.topics pluginName handlerMap) s $ Processor.process handlerMap


runDetectDuplicate :: String -> Util.Ping -> BusEndPoints -> s -> HandlerMap s IO -> IO (Either String ())
runDetectDuplicate pluginName ping = runWithInit pluginName (Util.quitIfExists pluginName ping)
