{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Empire.Env where

import           Prologue

import           Control.Concurrent.STM.TChan  (TChan)
import           Control.Concurrent.MVar       (MVar)
import           Empire.Data.Graph             (Graph, ClsGraph)
import qualified Empire.Empire                 as Empire
import           Luna.Compilation              (CompiledModules)
import           LunaStudio.API.AsyncUpdate    (AsyncUpdate)
import           LunaStudio.Data.GraphLocation (GraphLocation (..))
import           ZMQ.Bus.Config                (Config)
import qualified ZMQ.Bus.Config                as Config
import           ZMQ.Bus.Data.Message          (Message)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv   :: Empire.Env
               , _empireNotif :: Empire.CommunicationEnv
               , _formatted   :: Bool
               , _toBusChan   :: TChan Message
               , _projectRoot :: FilePath
               , _config      :: Config
               } deriving (Show)
makeLenses ''Env

make :: TChan Message
     -> TChan AsyncUpdate
     -> MVar (GraphLocation, ClsGraph, Bool)
     -> MVar Empire.SymbolMap
     -> MVar CompiledModules
     -> FilePath
     -> IO Env
make toBus fromEmpire tc sm imps fp = do
    zmqConfig <- Config.load
    return $ Env def (Empire.CommunicationEnv fromEmpire tc sm imps) True toBus fp zmqConfig

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
