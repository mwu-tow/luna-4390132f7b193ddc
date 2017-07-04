{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections    #-}

module Empire.ASTOps.BreadcrumbHierarchy where

import Empire.Prelude

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (forM)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.UUID.V4  as UUID

import           Empire.ASTOp                    (GraphOp)
import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Read              as ASTRead
import           Empire.Commands.Code            (addCodeMarker)
import           Empire.Data.AST                 (NodeRef)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (NodeIdCache, breadcrumbHierarchy, nodeIdMap)
import           Empire.Data.Layers              (Marker)
import           LunaStudio.Data.Node            (NodeId)
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.PortRef         (OutPortRef (..))

import qualified Luna.IR as IR

makeTopBreadcrumbHierarchy :: GraphOp m => NodeIdCache -> NodeRef -> m BH.TopItem
makeTopBreadcrumbHierarchy nodeIdCache ref = do
    let bareItem = BH.TopItem def $ Just ref
    children <- childrenFromSeq nodeIdCache ref
    return $ bareItem & BH.children .~ children

getMarker :: GraphOp m => NodeRef -> m Word64
getMarker marker = do
    IR.matchExpr marker $ \case
        IR.Marker index -> return index

childrenFromSeq :: GraphOp m => NodeIdCache -> NodeRef -> m (Map NodeId BH.BChild)
childrenFromSeq nodeIdCache ref = do
    IR.matchExpr ref $ \case
        IR.Seq    l r    -> Map.union <$> (childrenFromSeq nodeIdCache =<< IR.source l) <*> (childrenFromSeq nodeIdCache =<< IR.source r)
        IR.Marked m expr -> do
            expr'      <- IR.source expr
            index      <- getMarker =<< IR.source m
            newNodeId  <- liftIO UUID.nextRandom
            let nodeId = nodeIdCache ^. nodeIdMap . at index
                uid    = fromMaybe newNodeId nodeId
            childTarget <- IR.matchExpr expr' $ \case
                IR.Unify l r -> do
                    ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
                    IR.source r
                _ -> do
                    IR.putLayer @Marker expr' $ Just $ OutPortRef (NodeLoc def uid) []
                    return expr'
            child <- prepareChild nodeIdCache ref childTarget
            return $ Map.singleton uid child
        _ -> childrenFromSeq nodeIdCache =<< addCodeMarker ref

lambdaChildren :: GraphOp m => NodeIdCache -> NodeRef -> m (Map NodeId BH.BChild)
lambdaChildren nodeIdCache ref = IR.matchExpr ref $ \case
    IR.Seq l r -> Map.union <$> (childrenFromSeq nodeIdCache =<< IR.source l) <*> (lambdaChildren nodeIdCache =<< IR.source r)
    _          -> do
        marker <- IR.getLayer @Marker ref
        case marker of
            Just a  -> return Map.empty
            Nothing -> childrenFromSeq nodeIdCache ref

prepareChild :: GraphOp m => NodeIdCache -> NodeRef -> NodeRef -> m BH.BChild
prepareChild nodeIdCache marked ref = do
    isLambda <- ASTRead.isLambda ref
    (if isLambda then fmap BH.LambdaChild .: (prepareLambdaChild nodeIdCache) else prepareExprChild nodeIdCache) marked ref

prepareChildWhenLambda :: GraphOp m => NodeIdCache -> NodeRef -> NodeRef -> m (Maybe BH.LamItem)
prepareChildWhenLambda nodeIdCache marked ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild nodeIdCache marked ref else return Nothing

prepareLambdaChild :: GraphOp m => NodeIdCache -> NodeRef -> NodeRef -> m BH.LamItem
prepareLambdaChild nodeIdCache marked ref = do
    portMapping <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    lambdaBody  <- ASTRead.getFirstNonLambdaRef ref
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    children    <- lambdaChildren nodeIdCache lambdaBody
    newBody     <- ASTRead.getFirstNonLambdaRef ref
    return $ BH.LamItem portMapping marked children newBody

prepareExprChild :: GraphOp m => NodeIdCache -> NodeRef -> NodeRef -> m BH.BChild
prepareExprChild nodeIdCache marked ref = do
    let bareItem = BH.ExprItem Map.empty marked
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry (prepareChildWhenLambda nodeIdCache) . (id &&& id)) args
    let addItem par (port, child) = case child of
          Just ch -> par & BH.portChildren . at port ?~ ch
          _       -> par
    return $ BH.ExprChild $ foldl addItem bareItem $ zip [0..] items

restorePortMappings :: GraphOp m => Map (NodeId, Maybe Int) (NodeId, NodeId) -> m ()
restorePortMappings previousPortMappings = do
    hierarchy <- use breadcrumbHierarchy

    let goParent (BH.ToplevelParent topItem) = BH.ToplevelParent <$> goTopItem topItem
        goParent (BH.LambdaParent         _) = $notImplemented

        goBChild nodeId (BH.ExprChild exprItem)  = BH.ExprChild <$> goExprItem nodeId exprItem
        goBChild nodeId (BH.LambdaChild lamItem) = BH.LambdaChild <$> goLamItem (nodeId, Nothing) lamItem

        goTopItem (BH.TopItem childNodes body) = do
            updatedChildren <- mapM (\(a, b) -> (a,) <$> goBChild a b) $ Map.assocs childNodes
            return $ BH.TopItem (Map.fromList updatedChildren) body

        goLamItem idArg (BH.LamItem mapping marked children body) = do
            let cache       = Map.lookup idArg previousPortMappings
            updatedChildren <- forM cache $ \prev -> do
                ref <- ASTRead.getTargetFromMarked marked
                ASTBuilder.attachNodeMarkersForArgs (fst prev) [] ref
                updatedChildren <- mapM (\(a, b) -> (a,) <$> goBChild a b) $ Map.assocs children
                return $ Map.fromList updatedChildren
            return $ BH.LamItem (fromMaybe mapping cache) marked (fromMaybe children updatedChildren) body

        goExprItem nodeId (BH.ExprItem children self) = do
            updatedChildren <- mapM (\(a, b) -> (a,) <$> goLamItem (nodeId, Just a) b) $ Map.assocs children
            return $ BH.ExprItem (Map.fromList updatedChildren) self

    newHierarchy <- goParent hierarchy
    breadcrumbHierarchy .= newHierarchy
