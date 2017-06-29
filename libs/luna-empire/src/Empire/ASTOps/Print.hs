{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.ASTOps.Print (
    getTypeRep
  , printExpression
  , printName
  , printCurrentFunction
  , printNodeTarget
  ) where

import           Control.Monad                  (forM, (<=<))
import           Data.Char                      (isAlpha)
import           Data.List                      (delete, dropWhileEnd)
import           Empire.Prelude                 hiding (TypeRep)

import           Empire.ASTOp              (ASTOpReq, ASTOp, GraphOp, match)
import           Empire.Data.AST           (NodeRef)
import           Empire.Data.Graph         (Graph)
import qualified Empire.ASTOps.Read        as ASTRead
import qualified Empire.ASTOps.Deconstruct as ASTDeconstruct
import           LunaStudio.Data.Node      (NodeId)
import           LunaStudio.Data.TypeRep
import qualified Luna.IR                        as IR
import           Luna.IR.Term.Uni

import           Luna.Syntax.Text.Pretty.Pretty as CodeGen

getTypeRep :: GraphOp m => NodeRef -> m TypeRep
getTypeRep tp = match tp $ \case
    Monadic s _   -> getTypeRep =<< IR.source s
    Cons   n args -> TCons (nameToString n) <$> mapM (getTypeRep <=< IR.source) args
    Lam    a out  -> TLam <$> (getTypeRep =<< IR.source a) <*> (getTypeRep =<< IR.source out)
    Acc    t n    -> TAcc (nameToString n) <$> (getTypeRep =<< IR.source t)
    Var    n      -> return $ TVar $ delete '#' $ nameToString n
    Number _      -> return $ TCons "Number" []
    _             -> return TStar

parenIf :: Bool -> String -> String
parenIf False s = s
parenIf True  s = "(" ++ s ++ ")"

printCurrentFunction :: GraphOp m => m (Maybe (String, String))
printCurrentFunction = do
    mptr <- ASTRead.getCurrentASTPointer
    mlam <- ASTRead.getCurrentASTTarget
    forM ((,) <$> mptr <*> mlam) $ \(ptr, lam) -> do
        header <- printFunctionHeader ptr
        ret    <- printReturnValue lam
        return (header, ret)

printFunctionArguments :: GraphOp m => NodeRef -> m [String]
printFunctionArguments lam = match lam $ \case
    Grouped g   -> IR.source g >>= printFunctionArguments
    Lam _args _ -> do
        args' <- ASTDeconstruct.extractArguments lam
        mapM printExpression args'

printReturnValue :: GraphOp m => NodeRef -> m String
printReturnValue lam = do
    out' <- ASTRead.getLambdaOutputRef lam
    printExpression out'

printFunctionHeader :: GraphOp m => NodeRef -> m String
printFunctionHeader function = match function $ \case
    Unify l r -> do
        name <- IR.source l >>= printExpression
        args <- IR.source r >>= printFunctionArguments
        return $ "def " ++ name ++ " " ++ unwords args ++ ":"

instance ASTOpReq Graph m => Compactible t CompactStyle m where
    shouldBeCompact _ r = ASTRead.isGraphNode r

printExpression :: GraphOp m => NodeRef -> m String
printExpression = fmap convert . CodeGen.subpass CompactStyle . IR.unsafeGeneralize

printName :: GraphOp m => NodeRef -> m String
printName = fmap convert . CodeGen.subpass SimpleStyle . IR.unsafeGeneralize

printNodeTarget :: GraphOp m => NodeRef -> m String
printNodeTarget ref = match ref $ \case
    Unify _ r -> printExpression =<< IR.source r
    _         -> printExpression ref
