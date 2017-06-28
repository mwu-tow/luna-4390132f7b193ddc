{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.AST where

import           Control.Arrow                     (second)
import           Control.Monad.State
import           Data.Function                     (on)
import           Data.List                         (sortBy)
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Text                         as Text
import           Empire.Prelude

import qualified LunaStudio.Data.Error             as APIError
import           LunaStudio.Data.Node              (NodeId)
import           LunaStudio.Data.NodeMeta          (NodeMeta)
import           LunaStudio.Data.TypeRep           (TypeRep)
import           Empire.Data.AST                   (NodeRef, NotLambdaException(..), NotUnifyException(..))
import           Empire.Data.Layers                (Meta, TypeLayer, SpanOffset, SpanLength)

import           Empire.ASTOp                      (ASTOp, match)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Parse               as Parser
import qualified Empire.ASTOps.Print               as Printer
import qualified Empire.ASTOps.Read                as ASTRead
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan       as CodeSpan
import           Data.Text.Span                   (LeftSpacedSpan(..), SpacedSpan(..), leftSpacedSpan)
import           Data.Text.Position               (Delta(..))

import qualified Luna.IR as IR
import           Luna.IR.Term.Uni
import qualified OCI.IR.Repr.Vis as Vis

import           Web.Browser                       (openBrowser)

addNode :: ASTOp m => NodeId -> Maybe Text -> NodeRef -> m (NodeRef, Maybe Text)
addNode nid name node = do
    ASTBuilder.makeNodeRep nid name node

readMeta :: ASTOp m => NodeRef -> m (Maybe NodeMeta)
readMeta ref = IR.getLayer @Meta ref

writeMeta :: ASTOp m => NodeRef -> NodeMeta -> m ()
writeMeta ref newMeta = IR.putLayer @Meta ref $ Just newMeta

sortByPosition :: ASTOp m => [NodeId] -> m [NodeRef]
sortByPosition nodeIds = do
    refs  <- mapM ASTRead.getASTPointer nodeIds
    metas <- mapM readMeta refs
    let refsAndMetas = zip refs metas
        sorted       = sortBy (compare `on` snd) refsAndMetas
    return $ map (^. _1) sorted

makeSeq :: ASTOp m => [NodeRef] -> m (Maybe NodeRef)
makeSeq []     = return Nothing
makeSeq [node] = return $ Just node
makeSeq (n:ns) = Just <$> foldM f n ns
    where
        f :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
        f l r = IR.generalize <$> IR.seq l r

readSeq :: ASTOp m => NodeRef -> m [NodeRef]
readSeq node = match node $ \case
    Seq l r -> do
        previous  <- IR.source l >>= readSeq
        rightmost <- IR.source r
        return (previous ++ [rightmost])
    _       -> return [node]

getSeqs' :: ASTOp m => NodeRef -> m [NodeRef]
getSeqs' node = match node $ \case
    Seq l r -> do
        previous <- IR.source l >>= getSeqs'
        return $ previous ++ [node]
    _ -> return []

getSeqs :: ASTOp m => NodeRef -> m [NodeRef]
getSeqs node = match node $ \case
    Seq l r -> do
        previous <- IR.source l >>= getSeqs'
        return $ previous ++ [node]
    _ -> return [node]

previousNodeForSeq :: ASTOp m => NodeRef -> m (Maybe NodeRef)
previousNodeForSeq node = match node $ \case
    Seq l r -> do
        previousNode <- IR.source l
        match previousNode $ \case
            Seq l r -> Just <$> IR.source r
            _       -> return $ Just previousNode
    _ -> return Nothing

getLambdaInputRef :: ASTOp m => NodeRef -> Int -> m NodeRef
getLambdaInputRef node pos = do
    match node $ \case
        Grouped g      -> IR.source g >>= flip getLambdaInputRef pos
        Lam _args _out -> (!! pos) <$> ASTDeconstruct.extractArguments node
        _              -> throwM $ NotLambdaException node

isTrivialLambda :: ASTOp m => NodeRef -> m Bool
isTrivialLambda node = match node $ \case
    Grouped g -> IR.source g >>= isTrivialLambda
    Lam{} -> do
        args <- ASTDeconstruct.extractArguments node
        vars <- concat <$> mapM ASTRead.getVarsInside args
        out' <- ASTRead.getLambdaOutputRef node
        return $ out' `elem` vars
    _ -> throwM $ NotLambdaException node

dumpGraphViz :: ASTOp m => String -> m ()
dumpGraphViz name = Vis.snapshotWith nodeVis edgeVis name where
    edgeVis e = do
        off <- IR.getLayer @SpanOffset e
        return $ Just $ convert $ "[" ++ show (unwrap off) ++ "]"
    nodeVis n = do
        len <- IR.getLayer @SpanLength n
        return $ Just $ convert $ "[" ++ show (unwrap len) ++ "]"
