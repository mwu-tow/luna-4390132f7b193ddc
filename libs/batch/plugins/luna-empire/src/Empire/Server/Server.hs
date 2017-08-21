{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Empire.Server.Server where

import qualified Compress
import           Control.Concurrent.STM.TChan (writeTChan)
import           Control.Monad.Catch          (handle, try)
import           Control.Monad.State          (StateT)
import           Control.Monad.STM            (atomically)
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Bin
import           Data.ByteString.Lazy         (toStrict)
import           GHC.Stack                    (renderStack, whoCreated)
import           Prologue
import           System.Environment           (getEnv)
import           System.FilePath              (replaceFileName, (</>))

import qualified LunaStudio.API.Graph.Request as G
import           LunaStudio.API.Request       (Request(..))
import qualified LunaStudio.API.Response      as Response
import           LunaStudio.API.Topic         (MessageTopic)
import qualified LunaStudio.API.Topic         as Topic
import           LunaStudio.Data.GraphLocation (GraphLocation(..))
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified Empire.Commands.Persistence  as Persistence
import           Empire.Data.AST              (SomeASTException)
import           Empire.Empire                (Empire, runEmpire)
import           Empire.Env                   (Env)
import qualified Empire.Env                   as Env
import qualified System.Log.MLogger           as Logger
import qualified ZMQ.Bus.Config               as Config
import qualified ZMQ.Bus.Data.Message         as Message
import qualified ZMQ.Bus.EndPoint             as EP
import           ZMQ.Bus.Trans                (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

saveCurrentProject :: GraphLocation -> StateT Env BusT ()
saveCurrentProject loc = do
  currentEmpireEnv <- use Env.empireEnv
  empireNotifEnv   <- use Env.empireNotif
  projectRoot      <- use Env.projectRoot
  void $ liftIO $ runEmpire empireNotifEnv currentEmpireEnv $ Persistence.saveLocation projectRoot loc

sendToBus :: Binary a => String -> a -> StateT Env BusT ()
sendToBus topic bin = do
    chan <- use Env.toBusChan
    liftIO $ atomically $ writeTChan chan $ Message.Message topic $ Compress.pack $ Bin.encode bin

sendToBus' :: (MessageTopic a, Binary a) => a -> StateT Env BusT ()
sendToBus' msg = sendToBus (Topic.topic msg) msg

replyFail :: forall a b c. Response.ResponseResult a b c => Logger.Logger -> String -> Request a -> Response.Status b -> StateT Env BusT ()
replyFail logger errMsg req inv = do
  logger Logger.error $ formatErrorMessage req errMsg
  sendToBus' $ Response.error req inv errMsg

replyOk :: forall a b. Response.ResponseResult a b () => Request a -> b -> StateT Env BusT ()
replyOk req inv = sendToBus' $ Response.ok req inv

replyResult :: forall a b c. Response.ResponseResult a b c => Request a -> b -> c -> StateT Env BusT ()
replyResult req inv res = sendToBus' $ Response.result req inv res

errorMessage :: String
errorMessage = "Error processing request: "

formatErrorMessage :: MessageTopic a => a -> String -> String
formatErrorMessage req msg = errorMessage <> (Topic.topic req) <> ": " <> msg

defaultLibraryPath = "Main.luna"

webGUIHack :: G.GraphRequest req => req -> IO req
webGUIHack req = do
    lunaroot <- liftIO $ getEnv "LUNAROOT"
    let path = lunaroot </> "projects" </> defaultLibraryPath
        realLocation = req ^. G.location
        realFile     = realLocation ^. GraphLocation.filePath
        hackedReq    = if null realFile then req & G.location . GraphLocation.filePath .~ path
                                        else req
    return hackedReq

prettyException :: Exception e => e -> IO String
prettyException e = do
    stack <- whoCreated e
    return $ displayException e ++ "\n" ++ renderStack stack

modifyGraph :: forall req inv res res'. (G.GraphRequest req, Response.ResponseResult req inv res') => (req -> Empire inv) -> (req -> Empire res) -> (Request req -> inv -> res -> StateT Env BusT ()) -> Request req -> StateT Env BusT ()
modifyGraph inverse action success origReq@(Request uuid guiID request') = do
    request          <- liftIO $ webGUIHack request'
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    endPoints        <- EP.clientFromConfig <$> (liftIO Config.load)
    inv'             <- liftIO $ try $ runEmpire empireNotifEnv currentEmpireEnv $ inverse request
    case inv' of
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ prettyException exc
            replyFail logger err origReq (Response.Error err)
        Right (inv, _) -> do
            let invStatus = Response.Ok inv
            result <- liftIO $ try $ runEmpire empireNotifEnv currentEmpireEnv $ action request
            case result of
                Left  (exc :: SomeASTException) -> do
                    err <- liftIO $ prettyException exc
                    replyFail logger err origReq invStatus
                Right (result, newEmpireEnv) -> do
                    Env.empireEnv .= newEmpireEnv
                    success origReq inv result
                    saveCurrentProject $ request ^. G.location

modifyGraphOk :: forall req inv res . (Bin.Binary req, G.GraphRequest req, Response.ResponseResult req inv ()) => (req -> Empire inv) -> (req -> Empire res) -> Request req -> StateT Env BusT ()
modifyGraphOk inverse action = modifyGraph inverse action (\req@(Request uuid guiID request) inv _ -> replyOk req inv)

defInverse :: a -> Empire ()
defInverse = const $ return ()
