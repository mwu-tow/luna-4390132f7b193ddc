{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Empire.Commands.GraphBuilder where

import           Control.Monad.State             hiding (when)
import           Data.Foldable                   (toList)
import qualified Data.List                       as List
import           Data.Maybe                      (catMaybes, fromMaybe, maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Empire.ASTOp                    (ASTOp, match, runASTOp)
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Print             as Print
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.GraphUtils      as GraphUtils
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import           Empire.Data.Layers              (Marker, TypeLayer)
import           Empire.Empire
import           Empire.Prelude                  hiding (toList)
import qualified Luna.IR                         as IR
import qualified Luna.IR.Term.Literal            as Lit
import           Luna.IR.Term.Uni
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem, Named (..))
import qualified LunaStudio.Data.Graph           as API
import           LunaStudio.Data.LabeledTree     (LabeledTree (..))
import           LunaStudio.Data.MonadPath       (MonadPath (MonadPath))
import           LunaStudio.Data.Node            (NodeId)
import qualified LunaStudio.Data.Node            as API
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.Port            (InPort, InPortId, InPortIndex (..), InPortTree, InPorts (..), OutPort, OutPortId,
                                                  OutPortIndex (..), OutPortTree, OutPorts (..), Port (..), PortState (..))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..))
import           LunaStudio.Data.PortRef         (InPortRef (..), OutPortRef (..), srcNodeId)
import           LunaStudio.Data.TypeRep         (TypeRep (TCons, TStar))


decodeBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs bs@(Breadcrumb items) = runASTOp $ do
    bh    <- use Graph.breadcrumbHierarchy
    names <- forM (BH.getBreadcrumbItems bh bs) $ \child -> getUniName $ child ^. BH.self
    return $ Breadcrumb $ fmap (\(n, i) -> Named (fromMaybe "" n) i) $ zip names items

data CannotEnterNodeException = CannotEnterNodeException NodeId
    deriving Show
instance Exception CannotEnterNodeException where
    toException = astExceptionToException
    fromException = astExceptionFromException

buildGraph :: ASTOp m => m API.Graph
buildGraph = do
    connections <- buildConnections
    nodes       <- buildNodes
    edges       <- buildEdgeNodes
    API.Graph nodes connections (fst <$> edges) (snd <$> edges) <$> buildMonads

buildNodes :: ASTOp m => m [API.ExpressionNode]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes      <- mapM buildNode allNodeIds
    return nodes

buildMonads :: ASTOp m => m [MonadPath]
buildMonads = do
    allNodeIds <- getNodeIdSequence
    ioPath     <- filterM doesIO allNodeIds
    let ioMonad = MonadPath (TCons "IO" []) ioPath
    return [ioMonad]

doesIO :: ASTOp m => NodeId -> m Bool
doesIO node = do
    ref <- ASTRead.getASTPointer node
    tp  <- IR.getLayer @TypeLayer ref >>= IR.source
    IR.matchExpr tp $ \case
        Monadic _ m -> hasIO =<< IR.source m
        _           -> return False

hasIO :: ASTOp m => NodeRef -> m Bool
hasIO ref = IR.matchExpr ref $ \case
    Cons n _  -> return $ n == "IO"
    Unify l r -> (||) <$> (hasIO =<< IR.source l) <*> (hasIO =<< IR.source r)
    _         -> return False

getNodeSeq :: ASTOp m => m (Maybe NodeRef)
getNodeSeq = do
    lref    <- ASTRead.getCurrentASTTarget
    case lref of
        Just l -> ASTRead.getLambdaBodyRef l
        _      -> preuse $ Graph.breadcrumbHierarchy . BH.body

getNodeIdSequence :: ASTOp m => m [NodeId]
getNodeIdSequence = do
    bodySeq    <- getNodeSeq
    nodeSeq    <- case bodySeq of
        Just b -> AST.readSeq b
        _      -> return []
    catMaybes <$> mapM (getMarkedExpr >=> ASTRead.safeGetVarNodeId) nodeSeq

getMarkedExpr :: ASTOp m => NodeRef -> m NodeRef
getMarkedExpr ref = match ref $ \case
    IR.Marked _m expr -> IR.source expr
    _                 -> return ref

type EdgeNodes = (API.InputSidebar, API.OutputSidebar)

buildEdgeNodes :: ASTOp m => m (Maybe EdgeNodes)
buildEdgeNodes = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge  <- buildInputSidebar  inputPort
        outputEdge <- buildOutputSidebar outputPort
        return $ Just (inputEdge, outputEdge)
    _ -> return Nothing

getEdgePortMapping :: (MonadIO m, ASTOp m) => m (Maybe (NodeId, NodeId))
getEdgePortMapping = preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping

aliasPortName :: Text
aliasPortName = "alias"

selfPortName :: Text
selfPortName = "self"

buildNode :: ASTOp m => NodeId -> m API.ExpressionNode
buildNode nid = do
    root      <- GraphUtils.getASTPointer nid
    ref       <- GraphUtils.getASTTarget  nid
    expr      <- Text.pack <$> Print.printExpression ref
    marked    <- ASTRead.getASTRef nid
    meta      <- fromMaybe def <$> AST.readMeta marked
    name      <- getNodeName nid
    canEnter  <- ASTRead.isLambda ref
    inports   <- buildInPorts nid ref [] aliasPortName
    outports  <- buildOutPorts root
    code      <- getNodeCode nid
    return $ API.ExpressionNode nid expr name code inports outports meta canEnter

buildNodeTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate nid = do
  root     <- GraphUtils.getASTPointer nid
  ref      <- GraphUtils.getASTTarget  nid
  inPorts  <- buildInPorts nid ref [] aliasPortName
  outPorts <- buildOutPorts root
  return $ API.ExpressionUpdate nid inPorts outPorts

getUniName :: ASTOp m => NodeRef -> m (Maybe Text)
getUniName root = do
    root'  <- getMarkedExpr root
    match' <- ASTRead.isMatch root'
    if match' then do
        vnode <- ASTRead.getVarNode root'
        Just . Text.pack <$> Print.printName vnode
    else return Nothing

getNodeName :: ASTOp m => NodeId -> m (Maybe Text)
getNodeName nid = ASTRead.getASTPointer nid >>= getUniName

getNodeCode :: ASTOp m => NodeId -> m Text
getNodeCode nid = do
    ref <- ASTRead.getASTTarget nid
    Code.getCodeOf ref

getDefault :: ASTOp m => NodeRef -> m (Maybe PortDefault)
getDefault arg = match arg $ \case
        IR.String s       -> return $ Just $ Constant $ StringValue $ s
        IR.Number i       -> return $ Just $ Constant $ if Lit.isInteger i then IntValue $ Lit.toInt i else DoubleValue $ Lit.toDouble i
        IR.Cons "True"  _ -> return $ Just $ Constant $ BoolValue True
        IR.Cons "False" _ -> return $ Just $ Constant $ BoolValue False
        IR.Blank          -> return $ Nothing
        _                 -> Just . Expression . Text.unpack <$> Print.printFullExpression arg

getInPortDefault :: ASTOp m => NodeRef -> Int -> m (Maybe PortDefault)
getInPortDefault ref pos = do
    (_, args)  <- ASTDeconstruct.deconstructApp ref
    let argRef = args ^? ix pos
    join <$> mapM getDefault argRef

getPortState :: ASTOp m => NodeRef -> m PortState
getPortState node = do
    isConnected <- ASTRead.isGraphNode node
    if isConnected then return Connected else match node $ \case
        IR.String s     -> return . WithDefault . Constant . StringValue $ s
        IR.Number i     -> return . WithDefault . Constant $ if Lit.isInteger i then IntValue $ Lit.toInt i else DoubleValue $ Lit.toDouble i
        Cons n _ -> do
            name <- pure $ nameToString n
            case name of
                "False" -> return . WithDefault . Constant . BoolValue $ False
                "True"  -> return . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression . Text.unpack <$> Print.printFullExpression node
        Blank -> return NotConnected
        _     -> WithDefault . Expression . Text.unpack <$> Print.printFullExpression node

extractArgTypes :: ASTOp m => NodeRef -> m [TypeRep]
extractArgTypes node = do
    match node $ \case
        Monadic s _ -> extractArgTypes =<< IR.source s
        Lam arg out -> (:) <$> (Print.getTypeRep =<< IR.source arg) <*> (extractArgTypes =<< IR.source out)
        _           -> return []

safeGetVarName :: ASTOp m => NodeRef -> m (Maybe String)
safeGetVarName node = do
    name <- (Just <$> ASTRead.getVarName node) `catch`
        (\(e :: ASTRead.NoNameException) -> return Nothing)
    return name

extractArgNames :: ASTOp m => NodeRef -> m [Maybe String]
extractArgNames node = do
    match node $ \case
        Grouped g -> IR.source g >>= extractArgNames
        Lam{}  -> do
            insideLam  <- insideThisNode node
            args       <- ASTDeconstruct.extractArguments node
            vars       <- concat <$> mapM ASTRead.getVarsInside args
            let ports = if insideLam then vars else args
            mapM safeGetVarName ports
        -- App is Lam that has some args applied
        App{}  -> extractAppArgNames node
        Cons{} -> do
            vars  <- ASTRead.getVarsInside node
            names <- mapM ASTRead.getVarName vars
            return $ map Just names
        _ -> return []

extractAppArgNames :: ASTOp m => NodeRef -> m [Maybe String]
extractAppArgNames node = go [] node
    where
        go vars node = match node $ \case
            App f a -> do
                varName <- safeGetVarName =<< IR.source a
                go (varName : vars) =<< IR.source f
            Lam{}   -> extractArgNames node
            Cons{}  -> return vars
            Var{}   -> return vars
            Acc{}   -> return vars
            _       -> return []

insideThisNode :: ASTOp m => NodeRef -> m Bool
insideThisNode node = do
    curr <- ASTRead.getCurrentASTTarget
    return $ case curr of
        Just n -> n == node
        _      -> False

getPortsNames :: ASTOp m => NodeRef -> m [String]
getPortsNames node = do
    names <- extractArgNames node
    let backupNames = map (\i -> "arg" ++ show i) [(0::Int)..]
    forM (zip names backupNames) $ \(name, backup) -> return $ maybe backup id name

extractAppliedPorts :: ASTOp m => Bool -> Bool -> [NodeRef] -> NodeRef -> m [Maybe (TypeRep, PortState)]
extractAppliedPorts seenApp seenLam bound node = IR.matchExpr node $ \case
    Lam i o -> do
        inp   <- IR.source i
        nameH <- IR.matchExpr inp $ \case
            Var n -> return $ Just $ head $ convert n
            _     -> return Nothing
        case (seenApp, nameH) of
            (_, Just '#') -> extractAppliedPorts seenApp seenLam (inp : bound) =<< IR.source o
            (False, _)    -> extractAppliedPorts False   True    (inp : bound) =<< IR.source o
            _          -> return []
    App f a -> case seenLam of
        True  -> return []
        False -> do
            arg          <- IR.source a
            isB          <- ASTRead.isBlank arg
            argTp        <- IR.getLayer @TypeLayer arg >>= IR.source
            res          <- if isB || elem arg bound then return Nothing else Just .: (,) <$> Print.getTypeRep argTp <*> getPortState arg
            rest         <- extractAppliedPorts True False bound =<< IR.source f
            return $ res : rest
    _       -> return []


fromMaybePort :: Maybe (TypeRep, PortState) -> (TypeRep, PortState)
fromMaybePort Nothing  = (TStar, NotConnected)
fromMaybePort (Just p) = p

mergePortInfo :: [Maybe (TypeRep, PortState)] -> [TypeRep] -> [(TypeRep, PortState)]
mergePortInfo []             []       = []
mergePortInfo (p : rest)     []       = fromMaybePort p : mergePortInfo rest []
mergePortInfo []             (t : ts) = (t, NotConnected) : mergePortInfo [] ts
mergePortInfo (Nothing : as) (t : ts) = (t, NotConnected) : mergePortInfo as ts
mergePortInfo (Just a  : as) ts       = a : mergePortInfo as ts

extractPortInfo :: ASTOp m => NodeRef -> m [(TypeRep, PortState)]
extractPortInfo n = do
    applied  <- reverse <$> extractAppliedPorts False False [] n
    tp       <- IR.getLayer @TypeLayer n >>= IR.source
    fromType <- extractArgTypes tp
    return $ mergePortInfo applied fromType

buildArgPorts :: ASTOp m => InPortId -> NodeRef -> m [InPort]
buildArgPorts currentPort ref = do
    typed <- extractPortInfo ref
    names <- getPortsNames ref
    let portsTypes = fmap fst typed ++ List.replicate (length names - length typed) TStar
        psCons = zipWith3 Port
                          ((currentPort <>) . pure . Arg <$> [(0::Int)..])
                          (map Text.pack $ names ++ (("arg" ++) . show <$> [0..]))
                          portsTypes
    return $ zipWith ($) psCons (fmap snd typed ++ repeat NotConnected)

buildSelfPort :: ASTOp m => NodeId -> InPortId -> NodeRef -> m (Maybe (InPortTree InPort))
buildSelfPort nid currentPort node = do
    let potentialSelf = Port currentPort selfPortName TStar NotConnected
    match node $ \case
        Acc t _ -> do
            target <- IR.source t
            tree   <- buildInPorts nid target currentPort selfPortName
            return $ Just tree
        Var _     -> return $ Just $ LabeledTree def potentialSelf
        App f _   -> buildSelfPort nid currentPort =<< IR.source f
        Grouped g -> buildSelfPort nid currentPort =<< IR.source g
        _         -> return Nothing

buildWholePort :: ASTOp m => NodeId -> InPortId -> Text -> NodeRef -> m InPort
buildWholePort nid currentPort portName ref = do
    tp    <- followTypeRep ref
    pid   <- ASTRead.getNodeId ref
    state <- if pid == Just nid then return NotConnected else getPortState ref
    return $ Port currentPort portName tp state

followTypeRep :: ASTOp m => NodeRef -> m TypeRep
followTypeRep ref = do
    tp <- IR.source =<< IR.getLayer @TypeLayer ref
    Print.getTypeRep tp

buildInPorts :: ASTOp m => NodeId -> NodeRef -> InPortId -> Text -> m (InPortTree InPort)
buildInPorts nid ref currentPort portName = do
    selfPort <- buildSelfPort nid (currentPort ++ [Self]) ref
    argPorts <- buildArgPorts currentPort ref
    whole    <- buildWholePort nid currentPort portName ref
    return $ LabeledTree (InPorts selfPort def (LabeledTree def <$> argPorts)) whole

buildDummyOutPort :: ASTOp m => NodeRef -> m (OutPortTree OutPort)
buildDummyOutPort ref = do
    tp <- followTypeRep ref
    return $ LabeledTree (Port.OutPorts []) (Port [] "Output" tp NotConnected)

buildOutPortTree :: ASTOp m => OutPortId -> NodeRef -> m (OutPortTree OutPort)
buildOutPortTree portId ref' = do
    ref   <- ASTRead.cutThroughGroups ref'
    name  <- Print.printName ref
    tp    <- followTypeRep ref
    let wholePort = Port portId (Text.pack name) tp NotConnected
    children <- match ref $ \case
        Cons _ as -> zipWithM buildOutPortTree ((portId ++) . pure . Port.Projection <$> [0 ..]) =<< mapM IR.source as
        _         -> return []
    return $ LabeledTree (OutPorts children) wholePort

buildOutPorts :: ASTOp m => NodeRef -> m (OutPortTree OutPort)
buildOutPorts ref = match ref $ \case
    Unify l r -> buildOutPortTree [] =<< IR.source l
    _         -> buildDummyOutPort ref


buildConnections :: ASTOp m => m [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes       <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    edges          <- getEdgePortMapping
    connections    <- mapM getNodeInputs allNodes
    outputEdgeConn <- forM (snd <$> edges) getOutputSidebarInputs
    return $ (maybeToList $ join outputEdgeConn) ++ concat connections

buildInputSidebarTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildInputSidebarTypecheckUpdate nid = do
    API.InputSidebar nid ps <- buildInputSidebar nid
    return $ API.InputSidebarUpdate nid ps


buildInputSidebar :: ASTOp m => NodeId -> m API.InputSidebar
buildInputSidebar nid = do
    Just ref <- ASTRead.getCurrentASTTarget
    args     <- ASTDeconstruct.extractArguments ref
    argTrees <- zipWithM buildOutPortTree (pure . Projection <$> [0..]) args
    return $ API.InputSidebar nid argTrees

buildOutputSidebarTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildOutputSidebarTypecheckUpdate nid = do
    API.OutputSidebar nid m <- buildOutputSidebar nid
    return $ API.OutputSidebarUpdate nid m

buildOutputSidebar :: ASTOp m => NodeId -> m API.OutputSidebar
buildOutputSidebar nid = do
    Just ref <- ASTRead.getCurrentASTTarget
    out      <- ASTRead.getLambdaOutputRef ref
    tp       <- followTypeRep out
    state    <- getPortState  out
    return $ API.OutputSidebar nid $ LabeledTree (Port.InPorts Nothing Nothing [])  $ Port [] "output" tp state

getOutputSidebarInputs :: ASTOp m => NodeId -> m (Maybe (OutPortRef, InPortRef))
getOutputSidebarInputs outputEdge = do
    Just ref <- ASTRead.getCurrentASTTarget
    out      <- ASTRead.getLambdaOutputRef ref
    wholeIn  <- resolveInput out
    return $ (, InPortRef (NodeLoc def outputEdge) []) <$> wholeIn

nodeConnectedToOutput :: ASTOp m => m (Maybe NodeId)
nodeConnectedToOutput = do
    edges  <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping
    fmap join $ forM edges $ \(i, o) -> do
        connection <- getOutputSidebarInputs o
        return $ (view $ _1 . srcNodeId) <$> connection

resolveInput :: ASTOp m => NodeRef -> m (Maybe OutPortRef)
resolveInput = IR.getLayer @Marker

deepResolveInputs :: ASTOp m => NodeId -> NodeRef -> InPortRef -> m [(OutPortRef, InPortRef)]
deepResolveInputs nid ref portRef@(InPortRef loc id) = do
    currentPortResolution <- filter ((/= nid) . view srcNodeId) . toList <$> resolveInput ref
    let currentPortConn = (, portRef) <$> currentPortResolution
    args      <- reverse <$> ASTDeconstruct.extractAppArguments ref
    argsConns <- forM (zip args [0..]) $ \(arg, i) -> deepResolveInputs nid arg (InPortRef loc (id ++ [Arg i]))
    head      <- ASTDeconstruct.extractFun ref
    self      <- ASTDeconstruct.extractSelf head
    headConns <- case (self, head == ref) of
        (Just s, _) -> deepResolveInputs nid s    (InPortRef loc (id ++ [Self]))
        (_, False)  -> deepResolveInputs nid head (InPortRef loc (id ++ [Head]))
        _           -> return []
    return $ concat [currentPortConn, headConns, concat argsConns]

getNodeInputs :: ASTOp m => NodeId -> m [(OutPortRef, InPortRef)]
getNodeInputs nid = do
    let loc = NodeLoc def nid
    ref      <- ASTRead.getASTTarget   nid
    deepResolveInputs nid ref (InPortRef loc [])
