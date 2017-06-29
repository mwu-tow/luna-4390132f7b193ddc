{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Empire.ASTOps.BreadcrumbHierarchy where

import Empire.Prelude

import           Control.Arrow                 ((&&&))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.UUID.V4  as UUID

import           Empire.ASTOp                    (GraphOp)
import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Read              as ASTRead
import           Empire.Data.AST                 (NodeRef)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers              (Marker)
import           LunaStudio.Data.Node            (NodeId)
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.PortRef         (OutPortRef (..))

import qualified Luna.IR as IR


makeTopBreadcrumbHierarchy :: GraphOp m => NodeRef -> m BH.TopItem
makeTopBreadcrumbHierarchy ref = do
    let bareItem = BH.TopItem def $ Just ref
    children <- childrenFromSeq ref
    return $ bareItem & BH.children .~ children

childrenFromSeq :: GraphOp m => NodeRef -> m (Map NodeId BH.BChild)
childrenFromSeq ref = do
    IR.matchExpr ref $ \case
        IR.Seq    l r    -> Map.union <$> (childrenFromSeq =<< IR.source l) <*> (childrenFromSeq =<< IR.source r)
        IR.Marked _m expr -> do
            expr' <- IR.source expr
            uid   <- liftIO UUID.nextRandom
            childTarget <- IR.matchExpr expr' $ \case
                IR.Unify l r -> do
                    ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
                    IR.source r
                _ -> do
                    IR.putLayer @Marker expr' $ Just $ OutPortRef (NodeLoc def uid) []
                    return expr'
            child <- prepareChild ref childTarget
            return $ Map.singleton uid child
        IR.Unify l r -> do
            uid   <- liftIO UUID.nextRandom
            ASTBuilder.attachNodeMarkers uid []      =<< IR.source l
            child <- prepareChild ref =<< IR.source r
            return $ Map.singleton uid child
        _ -> do
            uid   <- liftIO UUID.nextRandom
            IR.putLayer @Marker ref $ Just $ OutPortRef (NodeLoc def uid) []
            child <- prepareChild ref ref
            return $ Map.singleton uid child

lambdaChildren :: GraphOp m => NodeRef -> m (Map NodeId BH.BChild)
lambdaChildren ref = IR.matchExpr ref $ \case
    IR.Seq l r -> Map.union <$> (childrenFromSeq =<< IR.source l) <*> (lambdaChildren =<< IR.source r)
    _          -> do
        marker <- IR.getLayer @Marker ref
        case marker of
            Just a  -> return Map.empty
            Nothing -> childrenFromSeq ref

prepareChild :: GraphOp m => NodeRef -> NodeRef -> m BH.BChild
prepareChild marked ref = do
    isLambda <- ASTRead.isLambda ref
    (if isLambda then fmap BH.LambdaChild .: prepareLambdaChild else prepareExprChild) marked ref

prepareChildWhenLambda :: GraphOp m => NodeRef -> NodeRef -> m (Maybe BH.LamItem)
prepareChildWhenLambda marked ref = do
    isLambda <- ASTRead.isLambda ref
    if isLambda then Just <$> prepareLambdaChild marked ref else return Nothing

prepareLambdaChild :: GraphOp m => NodeRef -> NodeRef -> m BH.LamItem
prepareLambdaChild marked ref = do
    portMapping  <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
    lambdaOutput <- ASTRead.getLambdaOutputRef   ref
    lambdaBody   <- ASTRead.getFirstNonLambdaRef ref
    ASTBuilder.attachNodeMarkersForArgs (fst portMapping) [] ref
    children     <- lambdaChildren lambdaBody
    return $ BH.LamItem portMapping marked children lambdaBody

prepareExprChild :: GraphOp m => NodeRef -> NodeRef -> m BH.BChild
prepareExprChild marked ref = do
    let bareItem = BH.ExprItem Map.empty marked
    args  <- ASTDeconstruct.extractAppArguments ref
    items <- mapM (uncurry prepareChildWhenLambda . (id &&& id)) args
    let addItem par (port, child) = case child of
          Just ch -> par & BH.portChildren . at port ?~ ch
          _       -> par
    return $ BH.ExprChild $ foldl addItem bareItem $ zip [0..] items
