{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Empire.Commands.Breadcrumb where

import           Empire.Prelude

import           Control.Monad                   (forM)
import           Control.Monad.Except            (throwError)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             (get, put)
import           Data.Coerce                     (coerce)
import           Data.Maybe                      (maybe)
import qualified Data.Map                        as Map
import qualified Data.UUID.V4                    as UUID

import           Empire.ASTOp                      (putNewIR, runAliasAnalysis, runASTOp)
import           Empire.ASTOps.BreadcrumbHierarchy as ASTBreadcrumb
import           Empire.Commands.AST               (classFunctions)
import           Empire.Commands.Code              (propagateLengths)
import           Empire.Data.AST                   (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.BreadcrumbHierarchy   (navigateTo, replaceAt)
import qualified Empire.Data.BreadcrumbHierarchy   as BH
import qualified Empire.Data.Graph                 as Graph
import qualified Empire.Data.Library               as Library

import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (..))
import           LunaStudio.Data.Library         (LibraryId)
import           LunaStudio.Data.Node            (NodeId)
import           LunaStudio.Data.Project         (ProjectId)

import           Empire.Commands.Library         (withLibrary)
import           Empire.Empire                   (Command, CommunicationEnv, Empire, runEmpire)

import qualified Luna.IR as IR

withBreadcrumb :: FilePath -> Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.ClsGraph a -> Empire a
withBreadcrumb file breadcrumb actG actC = withLibrary file $ zoomBreadcrumb breadcrumb actG actC

data BreadcrumbDoesNotExistException = BreadcrumbDoesNotExistException (Breadcrumb BreadcrumbItem)
    deriving (Show)

instance Exception BreadcrumbDoesNotExistException where
    toException = astExceptionToException
    fromException = astExceptionFromException

makeGraph :: NodeRef -> Maybe NodeId -> Command Library.Library (NodeId, Graph.Graph)
makeGraph fun lastUUID = zoom Library.body $ makeGraphCls fun lastUUID

makeGraphCls :: NodeRef -> Maybe NodeId -> Command Graph.ClsGraph (NodeId, Graph.Graph)
makeGraphCls fun lastUUID = do
    pmState   <- liftIO Graph.defaultPMState
    nodeCache <- use Graph.nodeCache
    (funName, IR.Rooted ir ref) <- runASTOp $ IR.matchExpr fun $ \case
        IR.ASGRootedFunction n root -> return (nameToString n, root)
    let ast   = Graph.AST ir pmState
    uuid <- maybe (liftIO $ UUID.nextRandom) return lastUUID
    let oldPortMapping = nodeCache ^. Graph.portMappingMap . at (uuid, Nothing)
    portMapping <- fromMaybeM (liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom) oldPortMapping
    let bh = BH.LamItem portMapping ref def ref
        graph = Graph.Graph ast bh 0 def def def
    Graph.clsFuns . at uuid ?= (funName, graph)
    withRootedFunction uuid $ do
        runASTOp $ propagateLengths ref
        runAliasAnalysis
        runASTOp $ do
            ASTBreadcrumb.makeTopBreadcrumbHierarchy nodeCache ref
            restorePortMappings (nodeCache ^. Graph.portMappingMap)
    return (uuid, graph)

runInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.Graph a
runInternalBreadcrumb breadcrumb act = do
    graph <- get
    let  breadcrumbHierarchy = graph ^. Graph.breadcrumbHierarchy
    case breadcrumbHierarchy `navigateTo` breadcrumb of
        Just h -> do
            env <- ask
            let newGraph = graph & Graph.breadcrumbHierarchy .~ h
            (res, state) <- liftIO $ runEmpire env newGraph act
            let modified = replaceAt breadcrumb breadcrumbHierarchy $ state ^. Graph.breadcrumbHierarchy
            mod <- maybe (throwM $ BreadcrumbDoesNotExistException breadcrumb) return modified
            let newGraph = state & Graph.breadcrumbHierarchy .~ mod
            put newGraph
            return res
        _ -> throwM $ BreadcrumbDoesNotExistException breadcrumb

zoomInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.Graph a
zoomInternalBreadcrumb (Breadcrumb (Definition _ : rest)) act = zoomInternalBreadcrumb (Breadcrumb rest) act
zoomInternalBreadcrumb breadcrumb act = runInternalBreadcrumb breadcrumb act

withRootedFunction :: NodeId -> Command Graph.Graph a -> Command Graph.ClsGraph a
withRootedFunction uuid act = do
    graph    <- preuse (Graph.clsFuns . ix uuid . _2) <?!> BreadcrumbDoesNotExistException (Breadcrumb [Definition uuid])
    env      <- ask
    clsGraph <- get
    let properGraph = let clsMarkers    = clsGraph ^. Graph.clsCodeMarkers
                          clsCode       = clsGraph ^. Graph.code
                          clsParseError = clsGraph ^. Graph.clsParseError
                      in graph & Graph.code .~ clsCode
                               & Graph.codeMarkers .~ clsMarkers
                               & Graph.parseError .~ clsParseError
    (res, newGraph) <- liftIO $ runEmpire env properGraph act
    let newClsGraph = clsGraph & Graph.clsFuns . ix uuid . _2 .~ newGraph
                               & Graph.clsCodeMarkers .~ newGraph ^. Graph.codeMarkers
                               & Graph.code           .~ newGraph ^. Graph.code
                               & Graph.clsParseError  .~ newGraph ^. Graph.parseError
    put newClsGraph
    return res

zoomBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.ClsGraph a -> Command Library.Library a
zoomBreadcrumb (Breadcrumb []) _actG actC = do
    env   <- ask
    lib   <- get
    let newLib = lib & Library.body . Graph.clsFuns .~ Map.empty
    (res, state) <- liftIO $ runEmpire env (lib ^. Library.body) actC
    put $ set Library.body state newLib
    return res
zoomBreadcrumb breadcrumb@(Breadcrumb (Definition uuid : rest)) actG _actC =
    zoom Library.body $ withRootedFunction uuid $ runInternalBreadcrumb (Breadcrumb rest) actG
zoomBreadcrumb breadcrumb _ _ = throwM $ BreadcrumbDoesNotExistException breadcrumb
