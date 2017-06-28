{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Empire.ASTOps.Builder where

import           Control.Monad                      (foldM, replicateM, forM_, zipWithM_)
import           Data.Maybe                         (isNothing)
import qualified Data.Text                          as Text
import           Empire.Prelude                     (stringToName)
import           Prologue

import           LunaStudio.Data.Node               (NodeId)
import           LunaStudio.Data.PortRef            (OutPortRef (..))
import           LunaStudio.Data.NodeLoc            (NodeLoc (..))
import qualified LunaStudio.Data.Port               as Port
import           Empire.ASTOp                       (ASTOp, match)
import           Empire.ASTOps.Deconstruct          (deconstructApp, extractArguments, dumpAccessors)
import           Empire.ASTOps.Remove               (removeSubtree)
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.Commands.Code               as Code
import           Empire.Data.AST                    (EdgeRef, NodeRef, astExceptionFromException,
                                                     astExceptionToException)
import           Empire.Data.Layers                 (Marker, SpanLength, SpanOffset)
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Data.BreadcrumbHierarchy    as BH

import           Data.Text.Position      (Delta)

import qualified OCI.IR.Combinators as IR (replace, replaceSource, deleteSubtree)
import           Luna.IR.Term.Uni
import qualified Luna.IR as IR


apps :: ASTOp m => IR.Expr f -> [NodeRef] -> m NodeRef
apps fun exprs = IR.unsafeRelayout <$> foldM f (IR.unsafeRelayout fun) (IR.unsafeRelayout <$> exprs)
    where
        f fun' arg' = appAny fun' arg'

appAny :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
appAny = fmap IR.generalize .: IR.app

lams :: ASTOp m => [NodeRef] -> NodeRef -> m NodeRef
lams args output = IR.unsafeRelayout <$> foldM (flip lamAny) (IR.unsafeRelayout output) (IR.unsafeRelayout <$> reverse args)

lamAny :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
lamAny a b = fmap IR.generalize $ IR.lam a b

newApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
newApplication fun arg' pos = do
    blanks <- sequence $ replicate pos IR.blank
    let args = IR.generalize blanks ++ [arg']
    apps fun args

rewireApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
rewireApplication fun arg' pos = do
    (target, oldArgs) <- deconstructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
    blanks <- replicateM (argsLength - length oldArgs) IR.blank
    let argsCmd = oldArgs ++ map IR.generalize blanks
        withNewArg = argsCmd & ix pos .~ arg'

    apps target withNewArg

replaceEdgeSource :: ASTOp m => EdgeRef -> Delta -> NodeRef -> m ()
replaceEdgeSource edge beg newSrc = do
    newCode <- getTgtCode newSrc
    oldSrc  <- IR.source edge
    oldLen  <- IR.getLayer @SpanLength oldSrc
    Code.applyDiff beg (beg + oldLen) newCode
    IR.replaceSource newSrc edge
    IR.deleteSubtree oldSrc
    Code.gossipLengthsChangedBy (fromIntegral (Text.length newCode) - oldLen) =<< IR.readTarget edge

countArgs :: ASTOp m => NodeRef -> m Int
countArgs expr = IR.matchExpr expr $ \case
    App f _ -> (+ 1) <$> (countArgs =<< IR.source f)
    _       -> return 0

ensureArgsCount :: ASTOp m => EdgeRef -> Delta -> Int -> m ()
ensureArgsCount e beg num = do
    argCount <- countArgs =<< IR.source e
    padArgs e beg (num - argCount)

padArgs :: ASTOp m => EdgeRef -> Delta -> Int -> m ()
padArgs e beg i | i <= 0    = return ()
                | otherwise = do
    bl     <- IR.blank
    rest   <- IR.source e
    ap     <- IR.generalize <$> IR.app rest bl
    [f, a] <- IR.inputs ap
    oldLen <- IR.getLayer @SpanLength rest
    IR.putLayer @SpanLength bl 1
    IR.putLayer @SpanLength ap (oldLen + 2)
    IR.putLayer @SpanOffset a  1
    Code.applyDiff (beg + oldLen) (beg + oldLen) " _"
    IR.replaceSource ap e
    Code.gossipLengthsChangedBy 2 =<< IR.readTarget e
    padArgs e beg $ pred i

getArgEdgeDroppedN :: ASTOp m => NodeRef -> Delta -> Int -> m (EdgeRef, Delta)
getArgEdgeDroppedN ref beg i = matchExpr ref $ \case
    App f a -> if i == 0
        then do
            off <- Code.getOffsetRelativeToTarget a
            return (a, beg + off)
        else do
            off <- Code.getOffsetRelativeToTarget f
            fun <- IR.source f
            getArgEdgeDroppedN fun (off + beg) (pred i)

applyFunction :: ASTOp m => EdgeRef -> Delta -> NodeRef -> Int -> m ()
applyFunction funE beg arg pos = do
    ensureArgsCount funE beg (pos + 1)
    f           <- IR.source funE
    count       <- countArgs f
    (edge, beg) <- getArgEdgeDroppedN f beg (count - pos - 1)
    replaceEdgeSource edge beg arg


data SelfPortNotExistantException = SelfPortNotExistantException NodeRef
    deriving (Show)

instance Exception SelfPortNotExistantException where
    toException = astExceptionToException
    fromException = astExceptionFromException

unfoldM :: ASTOp m => (a -> m (Either b a)) -> a -> m b
unfoldM f a = do
    res <- f a
    case res of
        Left  b -> return b
        Right b -> unfoldM f b

unfoldM' :: ASTOp m => (a -> m (Maybe a)) -> a -> m a
unfoldM' f a = do
    res <- f a
    case res of
        Nothing -> return a
        Just a' -> unfoldM' f a'

getCurrentAccTarget :: ASTOp m => EdgeRef -> Delta -> m (EdgeRef, Delta)
getCurrentAccTarget = curry $ unfoldM $ \(edge, codeBeg) -> do
    let passThrough e = Right . (e,) <$> ((+ codeBeg) <$> Code.getOffsetRelativeToTarget e)
    ref <- IR.source edge
    IR.matchExpr ref $ \case
        App f _ -> passThrough f
        Acc t _ -> passThrough t
        _       -> return $ Left (edge, codeBeg)

getTgtCode :: ASTOp m => NodeRef -> m Text
getTgtCode ref = IR.matchExpr ref $ \case
    Var n -> return $ convert n
    _     -> throwM $ SelfPortNotExistantException ref

ensureHasSelf :: ASTOp m => EdgeRef -> Delta -> m ()
ensureHasSelf e beg = IR.source e >>= flip IR.matchExpr `id` \case
    App f _ -> do
        off <- Code.getOffsetRelativeToTarget f
        ensureHasSelf f $ beg + off
    Acc _ _ -> return ()
    Var n'  -> do
        let n = convert n'
        bl <- IR.blank
        IR.putLayer @SpanLength bl 1
        ac <- IR.generalize <$> IR.acc bl n'
        IR.putLayer @SpanLength ac (4 + fromIntegral (Text.length n))
        oldTgt <- IR.source e
        IR.replaceSource ac e
        IR.deleteSubtree oldTgt
        Code.gossipLengthsChangedBy 4 =<< IR.readTarget e
        Code.insertAt beg "_ . "
        return ()
    _ -> throwM . SelfPortNotExistantException =<< IR.source e

makeAccessor :: ASTOp m => NodeRef -> EdgeRef -> Delta -> m ()
makeAccessor target naming exprBegin = do
    ensureHasSelf naming exprBegin
    (edge, tgtBegin) <- getCurrentAccTarget naming exprBegin
    replaceEdgeSource edge tgtBegin target

data SelfPortNotConnectedException = SelfPortNotConnectedException NodeRef
    deriving (Show)

instance Exception SelfPortNotConnectedException where
    toException = astExceptionToException
    fromException = astExceptionFromException

removeAccessor' :: ASTOp m => EdgeRef -> Delta -> m Bool
removeAccessor' ed beg = do
    expr <- IR.source ed
    IR.matchExpr expr $ \case
        App f _ -> do
            off <- Code.getOffsetRelativeToTarget f
            removeAccessor' f $ beg + off
        Acc t n -> do
            off <- Code.getOffsetRelativeToTarget t
            handled <- removeAccessor' t $ beg + off
            when (not handled) $ do
                length <- IR.getLayer @SpanLength expr
                v      <- IR.generalize <$> IR.var n
                let n' = convert n
                IR.putLayer @SpanLength v $ fromIntegral $ Text.length n'
                Code.applyDiff beg (beg + length) n'
                IR.replaceSource v ed
                IR.deleteSubtree expr
                Code.gossipLengthsChangedBy (fromIntegral (Text.length n') - length) =<< IR.readTarget ed
            return True
        _ -> return False

removeAccessor :: ASTOp m => EdgeRef -> Delta -> m ()
removeAccessor e beg = do
    handled <- removeAccessor' e beg
    when (not handled) $ throwM . SelfPortNotConnectedException =<< IR.source e

detachNodeMarkers :: ASTOp m => NodeRef -> m ()
detachNodeMarkers ref' = do
    ref <- ASTRead.cutThroughGroups ref'
    IR.putLayer @Marker ref Nothing
    inps <- IR.inputs ref
    mapM_ (IR.source >=> detachNodeMarkers) inps

attachNodeMarkers :: ASTOp m => NodeId -> Port.OutPortId -> NodeRef -> m ()
attachNodeMarkers marker port ref' = do
    ref <- ASTRead.cutThroughGroups ref'
    IR.putLayer @Marker ref $ Just $ OutPortRef (NodeLoc def marker) port
    match ref $ \case
        Cons _ as -> do
            args <- mapM IR.source as
            zipWithM_ (attachNodeMarkers marker) ((port ++) . pure . Port.Projection <$> [0..]) args
        _ -> return ()

detachNodeMarkersForArgs :: ASTOp m => NodeRef -> m ()
detachNodeMarkersForArgs lam = do
    args <- extractArguments lam
    mapM_ detachNodeMarkers args

attachNodeMarkersForArgs :: ASTOp m => NodeId -> Port.OutPortId -> NodeRef -> m ()
attachNodeMarkersForArgs nid port lam = do
    args <- extractArguments lam
    zipWithM_ (attachNodeMarkers nid) (pure . Port.Projection <$> [0..]) args

data CannotFlipNodeException = CannotFlipNodeException deriving (Show)
instance Exception CannotFlipNodeException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

patternify :: ASTOp m => NodeRef -> m NodeRef
patternify ref = match ref $ \case
    App _ _ -> do
        (fun, args) <- deconstructApp ref
        match fun $ \case
            Cons n _ -> do
                as <- mapM patternify args
                IR.generalize <$> IR.cons n as
            _ -> throwM CannotFlipNodeException
    Var n -> IR.generalize <$> IR.var n -- Copy the Var to forget all metadata like node markers, potentially belonging to other nodes and unified by Alias Analysis
    Grouped g -> patternify =<< IR.source g
    Number _  -> return ref
    String _  -> return ref
    Cons _ _  -> return ref
    _         -> throwM CannotFlipNodeException

appify :: ASTOp m => NodeRef -> m NodeRef
appify ref = match ref $ \case
    Cons n as -> do
        args <- mapM (IR.source >=> appify) as
        fun  <- IR.cons_ n
        apps fun args
    Var n     -> IR.generalize <$> IR.var n -- Copy the Var to forget all metadata like node markers, potentially belonging to other nodes and unified by Alias Analysis
    Number _  -> return ref
    String _  -> return ref
    Grouped g -> appify =<< IR.source g
    _         -> throwM CannotFlipNodeException

flipNode :: ASTOp m => NodeId -> m ()
flipNode nid = do
    lhs    <- ASTRead.getASTVar    nid
    rhs    <- ASTRead.getASTTarget nid
    newlhs <- patternify rhs
    newrhs <- appify     lhs
    uni    <- IR.unify newlhs newrhs
    attachNodeMarkers nid [] newlhs
    pointer <- ASTRead.getASTPointer nid
    IR.replace uni pointer

generateNodeName :: ASTOp m => m Text
generateNodeName = do
    lastNameId <- use Graph.lastNameId
    let newNameId = lastNameId + 1
    Graph.lastNameId .= newNameId
    return $ convert $ "node" <> show newNameId

makeNodeRep :: ASTOp m => NodeId -> Maybe Text -> NodeRef -> m (NodeRef, Maybe Text)
makeNodeRep marker name node = do
    (pat, uni, newName) <- match node $ \case
        Unify l r -> (, node, Nothing) <$> IR.source l
        _         -> do
            n   <- maybe generateNodeName pure name
            var <- IR.var' $ convert n
            IR.putLayer @SpanLength var (convert $ Text.length n)
            uni    <- IR.generalize <$> IR.unify var node
            [l, r] <- IR.inputs uni
            IR.putLayer @SpanOffset l 0
            IR.putLayer @SpanOffset r 3
            IR.putLayer @SpanLength uni =<< Code.computeLength uni
            return (IR.generalize var, IR.generalize uni, Just n)
    attachNodeMarkers marker [] pat
    return (uni, newName)
