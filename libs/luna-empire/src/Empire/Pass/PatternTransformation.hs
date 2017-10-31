{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Empire.Pass.PatternTransformation where

import           OCI.Pass        (SubPass, Pass, Inputs, Outputs, Preserves)

import Empire.Prelude hiding (Type, String, s, new, cons)
import qualified Luna.Prelude as P
import qualified OCI.IR.Repr.Vis as Vis
import Data.TypeDesc
import OCI.IR.Combinators
import Luna.IR hiding (expr)
import Luna.Pass.Data.ExprRoots
import OCI.Pass.Manager
import           Empire.Data.Layers   (Marker, Meta, TypeLayer, attachEmpireLayers, SpanLength, SpanOffset)
import           Data.Text.Position      (Delta)

import Data.TypeDesc

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Data.Map   (Map)

import System.Log

data PatternTransformation
type instance Abstract         PatternTransformation = PatternTransformation
type instance Inputs     Net   PatternTransformation = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer PatternTransformation = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // SpanLength, AnyExprLink // SpanOffset]
type instance Inputs     Attr  PatternTransformation = '[ExprRoots]
type instance Inputs     Event PatternTransformation = '[]

type instance Outputs    Net   PatternTransformation = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer PatternTransformation = '[AnyExpr // Model,  AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // SpanLength, AnyExprLink // SpanOffset]
type instance Outputs    Attr  PatternTransformation = '[]
type instance Outputs    Event PatternTransformation = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves        PatternTransformation = '[]

runPatternTransformation :: (MonadRef m, MonadPassManager m) => Pass PatternTransformation m
runPatternTransformation = do
    roots <- unwrap <$> getAttr @ExprRoots
    mapM_ transformPatterns roots

dumpConsApplication :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass PatternTransformation m (Name, [Expr Draft], [Link (Expr Draft) (Expr Draft)])
dumpConsApplication expr = matchExpr expr $ \case
    Grouped g -> dumpConsApplication =<< source g
    Cons n _  -> return (n, [], [])
    App f a   -> do
        (n, args, links) <- dumpConsApplication =<< source f
        arg <- source a
        return (n, arg : args, a:links)

-- computeLength :: GraphOp m => NodeRef -> m Delta
computeLength ref = do
    ins  <- inputs ref
    case ins of
        [] -> getLayer @SpanLength ref
        _  -> do
            ownLen <- matchExpr ref $ \case
                Cons n _ -> return $ length $ nameToString n
            offs <- P.mapM (getLayer @SpanOffset) ins
            lens <- P.mapM (getLayer @SpanLength <=< source) ins
            return $ fromIntegral ownLen <> mconcat offs <> mconcat lens

-- recomputeLength :: GraphOp m => NodeRef -> m ()
recomputeLength ref = putLayer @SpanLength ref =<< computeLength ref

gossipLengthsChanged :: (MonadRef m, MonadPassManager m) => SomeExpr -> SubPass PatternTransformation m ()
gossipLengthsChanged ref = do
    recomputeLength ref
    succs     <- Set.toList <$> getLayer @Succs ref
    succNodes <- mapM readTarget succs
    mapM_ gossipLengthsChanged succNodes

-- addToLength :: GraphOp m => NodeRef -> Delta -> m ()
addToLength ref delta = modifyLayer_ @SpanLength ref (+ delta)

gossipLengthsChangedBy :: (MonadRef m, MonadPassManager m) => Delta -> SomeExpr -> SubPass PatternTransformation m ()
gossipLengthsChangedBy delta ref = do
    addToLength ref delta
    succs     <- Set.toList <$> getLayer @Succs ref
    succNodes <- mapM readTarget succs
    mapM_ (gossipLengthsChangedBy delta) succNodes

flattenPattern :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass PatternTransformation m (Expr Draft)
flattenPattern expr = matchExpr expr $ \case
    Grouped g -> do
        a <- flattenPattern =<< source g
        gossipLengthsChangedBy 2 $ generalize a
        return a
    Var{}     -> return expr
    Cons{}    -> return expr
    App{}     -> do
        (name, children, links) <- dumpConsApplication expr
        flatChildren     <- mapM flattenPattern $ reverse children
        res <- generalize <$> cons name flatChildren
        childLinks <- inputs res
        P.forM (zip (reverse links) childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        gossipLengthsChanged $ generalize res
        return res
    _         -> return expr

transformPatterns :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass PatternTransformation m ()
transformPatterns expr = matchExpr expr $ \case
    Lam i o   -> do
        inp <- source i
        res <- flattenPattern inp
        replace res inp
        transformPatterns =<< source o
    Unify l r -> do
        left <- source l
        res  <- flattenPattern left
        replace res left
        transformPatterns =<< source r
    ASGFunction n as b -> do
        P.forM_ as $ \a' -> do
            a   <- source a'
            res <- flattenPattern a
            replace res a
        transformPatterns =<< source b
    _ -> mapM_ (transformPatterns <=< source) =<< inputs expr
