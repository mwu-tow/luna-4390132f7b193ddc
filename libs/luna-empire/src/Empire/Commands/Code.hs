{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}

module Empire.Commands.Code where

import           Prologue
import           Control.Monad.State     (MonadState)
import           Control.Monad           (forM)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.IO            as Text
import           Data.List               (sort)
import           Data.Maybe              (listToMaybe)
import           Empire.Data.Graph       as Graph
import           Empire.Empire           (Command, Empire)

import           Empire.Data.AST         (NodeRef, EdgeRef)
import           Empire.ASTOp            (ASTOp, runASTOp)
import           Empire.ASTOps.Read      as ASTRead
import qualified Luna.IR                 as IR
import           Data.Text.Position      (Delta)
import           Empire.Data.Layers      (SpanOffset, SpanLength)
import           Data.Text.Span          (LeftSpacedSpan(..), SpacedSpan(..), leftSpacedSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan, realSpan)

import           Luna.Syntax.Text.Lexer.Name (isOperator)
import qualified Luna.Syntax.Text.Lexer      as Lexer
import           Luna.Syntax.Text.SpanTree   as SpanTree
import           Data.VectorText             (VectorText)

import           LunaStudio.Data.Node               (NodeId)
import qualified Empire.Data.BreadcrumbHierarchy as BH

import           LunaStudio.Data.Point (Point(Point))

pointToDelta :: Point -> Text -> Delta
pointToDelta (Point col row) code = fromIntegral $ col + row + sumOfRowsBefore where
    sumOfRowsBefore = sum $ take row rowLengths
    rowLengths = Text.length <$> Text.lines code

deltaToPoint :: Delta -> Text -> Point
deltaToPoint delta code = Point col row where
    codePrefix = Text.take (fromIntegral delta + 1) code
    row = pred $ length $ Text.lines codePrefix
    col = pred $ Text.length $ Text.takeWhileEnd (/= '\n') codePrefix

removeMarkers :: Text -> Text
removeMarkers (convertVia @String -> code) = convertVia @String $ SpanTree.foldlSpans concatNonMarker "" spanTree where
    spanTree    = SpanTree.buildSpanTree code lexerStream
    lexerStream = Lexer.runLexer @Text code
    concatNonMarker t (Spanned span t1) = if span ^. spanType == MarkerSpan then t else t <> t1

viewDeltasToReal :: Text -> (Delta, Delta) -> (Delta, Delta)
viewDeltasToReal (convertVia @String -> code) (b, e) = if b == e then (bAf, bAf) else block where
    bAf         = SpanTree.viewToRealCursorAfterMarker spantree b
    block       = SpanTree.viewToRealBlock spantree (b, e)
    spantree    = SpanTree.buildSpanTree code lexerStream
    lexerStream = Lexer.runLexer @Text code

-- TODO: switch to Deltas exclusively
applyDiff :: (MonadState Graph m, Integral a) => a -> a -> Text -> m Text
applyDiff (fromIntegral -> start) (fromIntegral -> end) code = do
    currentCode <- use Graph.code
    let len            = end - start
        (prefix, rest) = Text.splitAt start currentCode
        prefix'        = if Text.length prefix < start
                            then Text.concat [prefix, Text.replicate (start - Text.length prefix) " "]
                            else prefix
        suffix         = Text.drop len rest
        newCode        = Text.concat [prefix', code, suffix]
    Graph.code .= newCode
    return newCode

insertAt :: MonadState Graph m => Delta -> Text -> m Text
insertAt at code = applyDiff at at code

removeAt :: MonadState Graph m => Delta -> Delta -> m ()
removeAt from to = void $ applyDiff from to ""

getAt :: MonadState Graph m => Delta -> Delta -> m Text
getAt (fromIntegral -> from) (fromIntegral -> to) = do
    code <- use Graph.code
    return $ Text.take (to - from) $ Text.drop from code

substituteLine :: Int -> Text -> Command Graph Text
substituteLine index newLine = do
    currentCode <- use Graph.code
    let codeLines = Text.lines currentCode
        newCode   = Text.unlines $ codeLines & ix index .~ newLine
    Graph.code .= newCode
    return newCode

getASTTargetBeginning :: ASTOp m => NodeId -> m Delta
getASTTargetBeginning id = do
    ref      <- ASTRead.getASTRef id
    Just beg <- getOffsetRelativeToFile ref
    IR.matchExpr ref $ \case
        IR.Marked _ b' -> do
            b    <- IR.source b'
            boff <- getOffsetRelativeToTarget b'
            IR.matchExpr b $ \case
                IR.Unify l r -> do
                    roff <- getOffsetRelativeToTarget r
                    return $ boff + roff + beg
                _ -> return $ beg + boff


removeLine :: Int -> Command Graph Text
removeLine index = do
    currentCode <- use Graph.code
    let codeLines = Text.lines currentCode
        newCode   = Text.unlines $ take index codeLines ++ drop (index + 1) codeLines
    Graph.code .= newCode
    return newCode

isOperatorVar :: ASTOp m => NodeRef -> m Bool
isOperatorVar expr = IR.matchExpr expr $ \case
    IR.Var n -> return $ isOperator n
    _        -> return False

getOffsetRelativeToTarget :: ASTOp m => EdgeRef -> m Delta
getOffsetRelativeToTarget edge = do
    ref  <- IR.readTarget edge
    let fallback = do
            inps <- IR.inputs ref
            let before = takeWhile (/= edge) inps
            lens <- forM before $ \e -> do
                off <- IR.getLayer @SpanOffset e
                len <- IR.getLayer @SpanLength =<< IR.source e
                return $ off <> len
            currentOff <- IR.getLayer @SpanOffset edge
            return $ currentOff <> foldl (<>) mempty lens
    let whenOp f a | a == edge = IR.getLayer @SpanOffset a
                   | otherwise = do
                       alen <- IR.getLayer @SpanLength =<< IR.source a
                       aoff <- IR.getLayer @SpanOffset a
                       foff <- IR.getLayer @SpanOffset f
                       return $ aoff <> alen <> foff
    IR.matchExpr ref $ \case
        IR.App f a -> do
            isOp <- isOperatorVar =<< IR.source f
            if isOp then whenOp f a else fallback
        IR.RightSection f a -> whenOp f a
        _ -> fallback


getOwnOffsetLength :: ASTOp m => NodeRef -> m (Maybe Delta)
getOwnOffsetLength ref = do
    succs <- toList <$> IR.getLayer @IR.Succs ref
    case succs of
        []  -> return Nothing
        [s] -> Just <$> IR.getLayer @SpanOffset s
        _   -> return Nothing

getOffsetRelativeToFile :: ASTOp m => NodeRef -> m (Maybe Delta)
getOffsetRelativeToFile ref = do
    begs <- getAllBeginningsOf ref
    case begs of
        [s] -> return $ Just s
        _   -> return Nothing

getAllBeginningsOf :: ASTOp m => NodeRef -> m [Delta]
getAllBeginningsOf ref = do
    succs <- toList <$> IR.getLayer @IR.Succs ref
    case succs of
        [] -> return [globalFileBlockStart]
        _  -> fmap concat $ forM succs $ \s -> do
            off  <- getOffsetRelativeToTarget s
            begs <- getAllBeginningsOf =<< IR.readTarget s
            return $ (off <>) <$> begs

getAnyBeginningOf :: ASTOp m => NodeRef -> m (Maybe Delta)
getAnyBeginningOf ref = listToMaybe <$> getAllBeginningsOf ref

getCodeOf :: ASTOp m => NodeRef -> m Text
getCodeOf ref = do
    Just beg <- getAnyBeginningOf ref
    len <- IR.getLayer @SpanLength ref
    getAt beg (beg + len)


replaceAllUses :: ASTOp m => NodeRef -> Text -> m ()
replaceAllUses ref new = do
    len         <- IR.getLayer @SpanLength ref
    occurrences <- getAllBeginningsOf ref
    let fromFileEnd = reverse $ sort occurrences
    forM_ fromFileEnd $ \beg -> applyDiff beg (beg + len) new
    gossipLengthsChangedBy (fromIntegral (Text.length new) - len) ref

computeLength :: ASTOp m => NodeRef -> m Delta
computeLength ref = do
    ins  <- IR.inputs ref
    offs <- mapM (IR.getLayer @SpanOffset) ins
    lens <- mapM (IR.getLayer @SpanLength <=< IR.source) ins
    return $ mconcat offs <> mconcat lens

recomputeLength :: ASTOp m => NodeRef -> m ()
recomputeLength ref = IR.putLayer @SpanLength ref =<< computeLength ref

-- TODO: read from upper AST
globalFileBlockStart :: Delta
globalFileBlockStart = 14

getCurrentBlockBeginning :: ASTOp m => m Delta
getCurrentBlockBeginning = do
    currentTgt <- ASTRead.getCurrentASTTarget
    body       <- preuse $ Graph.breadcrumbHierarchy . BH.body
    outLink    <- mapM ASTRead.getFirstNonLambdaLink currentTgt
    case (currentTgt, body) of
        (Nothing, Nothing) -> return globalFileBlockStart
        (Nothing, Just b)  -> fromJust <$> getOffsetRelativeToFile b
        (Just tgt, _)      -> do
            Just defBegin <- getOffsetRelativeToFile tgt
            off           <- getFirstNonLambdaOffset tgt
            return $ defBegin <> off

getFirstNonLambdaOffset :: ASTOp m => NodeRef -> m Delta
getFirstNonLambdaOffset ref = IR.matchExpr ref $ \case
    IR.Lam i o -> do
        ioff  <- IR.getLayer @SpanOffset i
        ooff  <- IR.getLayer @SpanOffset o
        ilen  <- IR.getLayer @SpanLength =<< IR.source i
        recur <- getFirstNonLambdaOffset =<< IR.source o
        return $ ioff + ooff + ilen + recur
    _ -> return 0

getCurrentBlockEnd :: ASTOp m => m Delta
getCurrentBlockEnd = do
    body       <- preuse $ Graph.breadcrumbHierarchy . BH.body
    case body of
        Nothing -> getCurrentBlockBeginning
        Just b  -> do
            len <- IR.getLayer @SpanLength b
            beg <- getCurrentBlockBeginning
            return $ len + beg

addLineAfter :: Int -> Text -> Command Graph Text
addLineAfter ((+1) -> index) line = do
    currentCode <- use Graph.code
    let codeLines = Text.lines currentCode
        newCode   = Text.unlines $ take index codeLines ++ [line] ++ drop index codeLines
    Graph.code .= newCode
    return newCode

defaultIndentationLength :: Delta
defaultIndentationLength = 4

getCurrentIndentationLength :: ASTOp m => m Delta
getCurrentIndentationLength = do
      o <- getCurrentBlockBeginning
      c <- use Graph.code
      return $ fromIntegral $ Text.length $ Text.takeWhileEnd (/= '\n') $ Text.take (fromIntegral o) c

propagateLengths :: ASTOp m => NodeRef -> m ()
propagateLengths node = do
    LeftSpacedSpan (SpacedSpan off len) <- fmap (view CodeSpan.realSpan) $ IR.getLayer @CodeSpan node
    IR.putLayer @SpanLength node len
    mapM_ propagateOffsets =<< IR.inputs node

propagateOffsets :: ASTOp m => EdgeRef -> m ()
propagateOffsets edge = do
    LeftSpacedSpan (SpacedSpan off len) <- fmap (view CodeSpan.realSpan) . IR.getLayer @CodeSpan =<< IR.readSource edge
    IR.putLayer @SpanOffset edge off
    propagateLengths =<< IR.readSource edge

gossipUsesChanged :: ASTOp m => NodeRef -> m ()
gossipUsesChanged ref = mapM_ gossipLengthsChanged =<< mapM IR.readTarget =<< (Set.toList <$> IR.getLayer @IR.Succs ref)

gossipUsesChangedBy :: ASTOp m => Delta -> NodeRef -> m ()
gossipUsesChangedBy delta ref = mapM_ (gossipLengthsChangedBy delta) =<< mapM IR.readTarget =<< (Set.toList <$> IR.getLayer @IR.Succs ref)

addToLength :: ASTOp m => NodeRef -> Delta -> m ()
addToLength ref delta = IR.modifyLayer_ @SpanLength ref (+ delta)

gossipLengthsChangedBy :: ASTOp m => Delta -> NodeRef -> m ()
gossipLengthsChangedBy delta ref = do
    addToLength ref delta
    succs     <- Set.toList <$> IR.getLayer @IR.Succs ref
    succNodes <- mapM IR.readTarget succs
    mapM_ (gossipLengthsChangedBy delta) succNodes

gossipLengthsChanged :: ASTOp m => NodeRef -> m ()
gossipLengthsChanged ref = do
    recomputeLength ref
    succs     <- Set.toList <$> IR.getLayer @IR.Succs ref
    succNodes <- mapM IR.readTarget succs
    mapM_ gossipLengthsChanged succNodes

makeMarker :: Word64 -> Text
makeMarker s = Text.pack $ "«" <> show s <> "»"
