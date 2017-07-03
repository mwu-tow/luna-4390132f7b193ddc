{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Empire.Commands.Typecheck where

import           Control.Arrow                    ((***))
import           Control.Concurrent               (forkIO, killThread)
import           Control.Monad                    (forM_, void)
import           Control.Monad.Except             hiding (when)
import           Control.Monad.Reader             (ask, runReaderT)
import           Control.Monad.State              (execStateT)
import qualified Data.Map                         as Map
import           Empire.Prelude                   hiding (toList)
import           Prologue                         (catMaybes)

import qualified LunaStudio.Data.Error            as APIError
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import           LunaStudio.Data.Node             (NodeId)
import           LunaStudio.Data.NodeValue        (NodeValue (..), VisualizationValue (..))

import           Empire.ASTOp                     (runASTOp, runTypecheck)
import qualified Empire.ASTOps.Read               as ASTRead
import           Empire.Commands.Breadcrumb       (zoomInternalBreadcrumb)
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Data.BreadcrumbHierarchy  (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Empire

import           Luna.Builtin.Data.Class          (Class (..))
import           Luna.Builtin.Data.LunaEff        (runError, runIO)
import           Luna.Builtin.Data.Module         (Imports (..), importedClasses)
import           Luna.Builtin.Prim                (SingleRep (..), ValueRep (..), getReps)
import qualified Luna.Compilation                 as Compilation
import qualified Luna.IR                          as IR
import           Luna.Pass.Data.ExprMapping
import qualified Luna.Pass.Evaluation.Interpreter as Interpreter


runTC :: Imports -> Command Graph ()
runTC imports = do
    runTypecheck imports
    runASTOp $ do
        mapping <- unwrap <$> IR.getAttr @ExprMapping
        Graph.breadcrumbHierarchy . BH.refs %= (\x -> Map.findWithDefault x x mapping)
    return ()

runInterpreter :: Imports -> Command Graph (Maybe Interpreter.LocalScope)
runInterpreter imports = runASTOp $ do
    bodyRef    <- preuse $ Graph.breadcrumbHierarchy . BH.body
    res        <- mapM (Interpreter.interpret' imports . IR.unsafeGeneralize) bodyRef
    case res of
        Nothing -> return Nothing
        Just v  -> do
            result <- liftIO $ runIO $ runError $ execStateT v def
            case result of
                Left e  -> return Nothing
                Right r -> return $ Just r

reportError :: GraphLocation -> NodeId -> Maybe APIError.Error -> Command InterpreterEnv ()
reportError loc nid err = do
    cachedErr <- uses errorsCache $ Map.lookup nid
    when (cachedErr /= err) $ do
        errorsCache %= Map.alter (const err) nid
        valuesCache %= Map.delete nid
        case err of
            Just e  -> Publisher.notifyResultUpdate loc nid (NodeError e)     0
            Nothing -> Publisher.notifyResultUpdate loc nid (NodeValue "" Nothing) 0

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc@(GraphLocation _ br) = zoom graph $ zoomInternalBreadcrumb br $ do
     portMapping <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping
     (updates, errors) <- runASTOp $ do
         sidebarUpdates <- case portMapping of
             Just (i, o) -> do
                 (u1, u2) <- (,) <$> GraphBuilder.buildInputSidebarTypecheckUpdate i <*> GraphBuilder.buildOutputSidebarTypecheckUpdate o
                 return [u1, u2]
             Nothing     -> return []
         allNodeIds  <- uses Graph.breadcrumbHierarchy topLevelIDs
         nodeUpdates <- mapM GraphBuilder.buildNodeTypecheckUpdate allNodeIds
         errors      <- forM allNodeIds $ \nid -> do
             errs <- IR.getLayer @IR.Errors =<< ASTRead.getASTRef nid
             case errs of
                 []     -> return Nothing
                 e : es -> return $ Just $ (nid, NodeError $ APIError.Error APIError.CompileError e)
         return (sidebarUpdates ++ nodeUpdates, errors)
     mapM_ (Publisher.notifyNodeTypecheck loc) updates
     forM_ (catMaybes errors) $ \(nid, e) -> Publisher.notifyResultUpdate loc nid e 0

updateMonads :: GraphLocation -> Command InterpreterEnv ()
updateMonads loc@(GraphLocation _ br) = return ()--zoom graph $ zoomBreadcrumb br $ do
    {-newMonads <- runASTOp GraphBuilder.buildMonads-}
    {-Publisher.notifyMonadsUpdate loc newMonads-}

updateValues :: GraphLocation -> Interpreter.LocalScope -> Command InterpreterEnv ()
updateValues loc scope = do
    childrenMap <- use $ graph . Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env     <- ask
    allVars <- zoom graph $ runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, tgt) -> do
        pointer <- ASTRead.getASTPointer nid
        IR.matchExpr pointer $ \case
            IR.Unify{} -> Just . (nid,) <$> ASTRead.getVarNode pointer
            _          -> return Nothing
    forM_ allVars $ \(nid, ref) -> do
        let resVal = Interpreter.localLookup (IR.unsafeGeneralize ref) scope
            send m = flip runReaderT env $ Publisher.notifyResultUpdate loc nid m 0
            sendRep (ErrorRep e)     = send $ NodeError $ APIError.Error APIError.RuntimeError $ convert e
            sendRep (SuccessRep s l) = send $ NodeValue (convert s) $ Value . convert <$> l
            sendStreamRep a@(ErrorRep _)   = sendRep a
            sendStreamRep (SuccessRep s l) = send $ NodeValue (convert s) $ StreamDataPoint . convert <$> l
        liftIO $ forM_ resVal $ \v -> do
            value <- getReps v
            case value of
                OneTime r   -> sendRep r
                Streaming f -> do
                    send (NodeValue "Stream" $ Just StreamStart)
                    void $ forkIO $ f sendStreamRep

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

newtype Scope = Scope Imports

createStdlib :: String -> IO (IO (), Scope)
createStdlib = fmap (id *** Scope) . Compilation.createStdlib

getSymbolMap :: Scope -> SymbolMap
getSymbolMap (Scope (Imports clss funcs)) = SymbolMap functions classes where
    functions = conses ++ (convert <$> Map.keys funcs)
    conses    = getConses =<< Map.elems clss
    getConses (Class conses _) = convert <$> Map.keys conses
    classes   = processClass <$> Map.mapKeys convert clss
    processClass (Class _ methods) = convert <$> Map.keys methods

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    std     <- use imports
    cln     <- use cleanUp
    threads <- use listeners
    liftIO $ print $ Map.keys $ std ^. importedClasses
    zoom graph $ runTC std
    updateNodes  loc
    {-updateMonads loc-}
    liftIO cln
    liftIO $ mapM killThread threads
    listeners .= []
    scope <- zoom graph $ runInterpreter std
    mapM_ (updateValues loc) scope
