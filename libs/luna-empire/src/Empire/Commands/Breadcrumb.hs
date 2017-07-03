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

makeGraph :: String -> Maybe NodeId -> Command Library.Library (NodeId, Graph.Graph)
makeGraph funName lastUUID = do
    env      <- ask
    emptyAST <- liftIO $ Graph.emptyAST
    lib      <- get
    let clsGraph = lib ^. Library.body
    (IR.Rooted ir ref, state) <- liftIO $ runEmpire env clsGraph $ do
        IR.Rooted fun ref <- runASTOp $ do
            funs      <- classFunctions $ clsGraph ^. Graph.clsClass
            foundFuns <- forM funs $ \fun -> IR.matchExpr fun $ \case
                IR.ASGRootedFunction n root -> return $ if n == (convert funName) then Just root else Nothing
            case catMaybes foundFuns of
                [f] -> return f
                _   -> throwM $ BreadcrumbDoesNotExistException (Breadcrumb [])
        return $ IR.Rooted fun ref
    let ast       = emptyAST & Graph.ir .~ ir
        tempBH    = def & BH._ToplevelParent . BH.topBody ?~ ref
        tempGraph = Graph.Graph ast tempBH 0 Map.empty "" def
    (_, graph) <- liftIO $ runEmpire env tempGraph $ do
        runASTOp $ propagateLengths ref
        runAliasAnalysis
        newBH <- runASTOp $ ASTBreadcrumb.makeTopBreadcrumbHierarchy ref
        Graph.breadcrumbHierarchy .= BH.ToplevelParent newBH
    uuid <- case lastUUID of
        Just id -> return id
        _       -> liftIO $ UUID.nextRandom
    Library.body . Graph.clsFuns . at uuid ?= (funName, graph)
    return (uuid, graph)

makeGraphCls :: NodeRef -> Maybe NodeId -> Command Graph.ClsGraph (NodeId, Graph.Graph)
makeGraphCls fun lastUUID = do
    emptyAST <- liftIO $ Graph.emptyAST
    clsClass <- use Graph.clsClass
    (name, IR.Rooted ir ref) <- runASTOp $ IR.matchExpr fun $ \case
        IR.ASGRootedFunction n root -> return (nameToString n, root)
    let ast       = emptyAST & Graph.ir .~ ir
        tempBH    = def & BH._ToplevelParent . BH.topBody ?~ ref
        tempGraph = Graph.Graph ast tempBH 0 Map.empty "" def
    env <- ask
    (_, graph) <- liftIO $ runEmpire env tempGraph $ do
        runASTOp $ propagateLengths ref
        runAliasAnalysis
        newBH <- runASTOp $ ASTBreadcrumb.makeTopBreadcrumbHierarchy ref
        Graph.breadcrumbHierarchy .= BH.ToplevelParent newBH
    uuid <- case lastUUID of
        Just id -> return id
        _       -> liftIO $ UUID.nextRandom
    Graph.clsFuns . at uuid ?= (name, graph)
    return (uuid, graph)


zoomInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.Graph a
zoomInternalBreadcrumb (Breadcrumb (Definition _ : rest)) act = zoomInternalBreadcrumb (Breadcrumb rest) act
zoomInternalBreadcrumb breadcrumb act = do
    graph <- get
    let  breadcrumbHierarchy = graph ^. Graph.breadcrumbHierarchy
    case breadcrumbHierarchy `navigateTo` breadcrumb of
        Just h -> do
            env <- ask
            let newGraph = graph & Graph.breadcrumbHierarchy .~ h
            (res, state) <- liftIO $ runEmpire env newGraph act
            let modified = replaceAt breadcrumb breadcrumbHierarchy $ state ^. Graph.breadcrumbHierarchy
            mod <- maybe (throwM $ BreadcrumbDoesNotExistException breadcrumb) return modified
            put $ state & Graph.breadcrumbHierarchy .~ mod
            return res
        _ -> throwM $ BreadcrumbDoesNotExistException breadcrumb

zoomBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.ClsGraph a -> Command Library.Library a
zoomBreadcrumb (Breadcrumb []) _actG actC = do
    env   <- ask
    lib   <- get
    let newLib = lib & Library.body . Graph.clsFuns .~ Map.empty
    (res, state) <- liftIO $ runEmpire env (lib ^. Library.body) actC
    put $ set Library.body state newLib
    return res
zoomBreadcrumb breadcrumb@(Breadcrumb (Definition uuid : rest)) actG _actC = do
    lib <- get
    env <- ask
    let graphCache = fmap snd $ lib ^. Library.body . Graph.clsFuns . at uuid
    graph <- case graphCache of
        Just g -> return g
        _      -> snd <$> makeGraph $notImplemented (Just uuid)
    let properGraph = let clsGraph   = lib ^. Library.body
                          clsMarkers = clsGraph ^. Graph.clsCodeMarkers
                          clsCode    = clsGraph ^. Graph.code
                          clsParseError = clsGraph ^. Graph.clsParseError
                      in graph & Graph.code .~ clsCode
                               & Graph.codeMarkers .~ clsMarkers
                               & Graph.parseError .~ clsParseError
    case rest of
        [] -> do
            (res, state) <- liftIO $ runEmpire env properGraph actG
            let newLib = lib & Library.body . Graph.clsFuns . at uuid . traverse . _2 .~ state
                             & Library.body . Graph.clsCodeMarkers .~ state ^. Graph.codeMarkers
                             & Library.body . Graph.code           .~ state ^. Graph.code
                             & Library.body . Graph.clsParseError  .~ state ^. Graph.parseError
            put newLib
            return res
        _  -> do
            let  breadcrumbHierarchy = properGraph ^. Graph.breadcrumbHierarchy
            case breadcrumbHierarchy `navigateTo` (Breadcrumb rest) of
                Just h -> do
                    let newGraph = properGraph & Graph.breadcrumbHierarchy .~ h
                    (res, state) <- liftIO $ runEmpire env newGraph actG
                    let modified = replaceAt (Breadcrumb rest) breadcrumbHierarchy $ state ^. Graph.breadcrumbHierarchy
                    mod <- maybe (throwM $ BreadcrumbDoesNotExistException breadcrumb) return modified
                    let properState = state & Graph.breadcrumbHierarchy .~ mod
                    let newLib = lib & Library.body . Graph.clsFuns . at uuid . traverse . _2 .~ properState
                                     & Library.body . Graph.clsCodeMarkers .~ properState ^. Graph.codeMarkers
                                     & Library.body . Graph.code           .~ properState ^. Graph.code
                                     & Library.body . Graph.clsParseError  .~ properState ^. Graph.parseError
                    put newLib
                    return res
                _ -> throwM $ BreadcrumbDoesNotExistException breadcrumb
zoomBreadcrumb breadcrumb _ _ = throwM $ BreadcrumbDoesNotExistException breadcrumb
