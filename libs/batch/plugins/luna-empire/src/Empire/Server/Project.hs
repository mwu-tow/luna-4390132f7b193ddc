{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Project where

import           Prologue

import           Control.Monad.State              (StateT)
import qualified LunaStudio.API.Project.CreateProject as CreateProject
import qualified LunaStudio.API.Project.ExportProject as ExportProject
import qualified LunaStudio.API.Project.ImportProject as ImportProject
import qualified LunaStudio.API.Project.ListProjects  as ListProjects
import qualified LunaStudio.API.Project.OpenProject   as OpenProject
import           LunaStudio.API.Request               (Request (..))
import qualified LunaStudio.API.Response              as Response
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Persistence      as Persistence
import qualified Empire.Data.Project              as DataProject
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (replyFail, replyResult, sendToBus')
import qualified System.Log.MLogger               as Logger
import           ZMQ.Bus.Trans                    (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)



handleOpenProject :: Request OpenProject.Request -> StateT Env BusT ()
handleOpenProject req@(Request _ _ request) = $_NOT_IMPLEMENTED


handleCreateProject :: Request CreateProject.Request -> StateT Env BusT ()
handleCreateProject req@(Request _ _ request) = $_NOT_IMPLEMENTED

handleListProjects :: Request ListProjects.Request -> StateT Env BusT ()
handleListProjects req = $_NOT_IMPLEMENTED

sendListProjectsUpdate :: StateT Env BusT ()
sendListProjectsUpdate = $_NOT_IMPLEMENTED

handleExportProject :: Request ExportProject.Request -> StateT Env BusT ()
handleExportProject req@(Request _ _ (ExportProject.Request projectId)) = $_NOT_IMPLEMENTED

handleImportProject :: Request ImportProject.Request -> StateT Env BusT ()
handleImportProject req@(Request _ _ (ImportProject.Request projectData)) = $_NOT_IMPLEMENTED
