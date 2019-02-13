{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Main where

import Prologue as P hiding (Either)
import Criterion.Main
import Control.Monad.State.Layered hiding ((.))
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy   as Lazy
import qualified Control.Monad.State.Class  as State
import Control.Monad.Codensity
import qualified Control.Monad.State.CPS as CPS
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Random
import Text.Parsert hiding ((.))
import Data.Either.Strict
import qualified Control.Monad.State.Strict2 as S2

-----------------------------
-- === Criterion utils === --
-----------------------------

eval :: NFData a => a -> IO a
eval = evaluate . force ; {-# INLINE eval #-}

liftExp :: (Int -> a) -> (Int -> a)
liftExp f = f . (10^) ; {-# INLINE liftExp #-}

expCodeGen :: NFData a => (Int -> a) -> (Int -> IO a)
expCodeGen f i = do
    putStrLn $ "generating input code (10e" <> show i <> " chars)"
    out <- eval $ liftExp f i
    putStrLn "code generated sucessfully"
    return out
{-# INLINE expCodeGen #-}

expCodeGenBench :: (NFData a, NFData b) => (Int -> a) -> (a -> b) -> Int -> Benchmark
expCodeGenBench f p i = env (expCodeGen f i) $ bench ("10e" <> show i) . nf p ; {-# INLINE expCodeGenBench #-}


-------------------------------
-- === (a*) list parsing === --
-------------------------------

genList_a :: Int -> [Char]
genList_a i = replicate i 'a' ; {-# INLINE genList_a #-}

pureListParser_a :: [Char] -> Bool
pureListParser_a = \case
    'a':s -> pureListParser_a s
    []    -> True
    _     -> False
{-# INLINE pureListParser_a #-}

mtlStateListParser_a :: State.MonadState [Char] m => m Bool
mtlStateListParser_a = State.get >>= \case
    'a':s -> State.put s >> mtlStateListParser_a
    []    -> return True
    _     -> return False
{-# INLINE mtlStateListParser_a #-}

mtlStateListParser_a_typed :: Strict.State [Char] Bool
mtlStateListParser_a_typed = State.get >>= \case
    'a':s -> State.put s >> mtlStateListParser_a_typed
    []    -> return True
    _     -> return False
{-# INLINE mtlStateListParser_a_typed #-}

mtlStateListParser_a_let :: Strict.MonadState [Char] m => m Bool
mtlStateListParser_a_let = go where
    go = Strict.get >>= \case
        'a':s -> Strict.put s >> go
        []    -> return True
        _     -> return False
{-# INLINE mtlStateListParser_a_let #-}

layeredStateListParser_a :: MonadState [Char] m => m Bool
layeredStateListParser_a = get @[Char] >>= \case
    'a':s -> put @[Char] s >> layeredStateListParser_a
    []    -> return True
    _     -> return False
{-# INLINE layeredStateListParser_a #-}

s2ListParser_a :: S2.State [Char] Bool
s2ListParser_a = S2.get' >>= \case
    'a':s -> S2.put' s >> s2ListParser_a
    []    -> return True
    _     -> return False
{-# INLINE s2ListParser_a #-}

parsertListParser_ab :: [Char] -> Either (NonEmpty String) [Char]
parsertListParser_ab s = runIdentity
                --  $ evalBacktracker
                 $ runFailParser
                 $ evalStreamProvider (listStream s)
                --  $ evalOffsetRegister
                 $ (many (token 'a' <|> token 'b') )


parsertListParser_a :: [Char] -> Either (NonEmpty String) [Char]
parsertListParser_a s = runIdentity
                --  $ evalBacktracker
                 $ runFailParser
                 $ evalStreamProvider (listStream s)
                --  $ evalOffsetRegister
                 $ (many (token 'a'))


instance (NFData l, NFData r) => NFData (Either l r)

-- {-# SPECIALIZE mtlStateListParser_a :: Strict.State [Char] Bool #-}
-- {-# SPECIALIZE mtlStateListParser_a_typed :: Strict.State [Char] Bool #-}


main = do
    -- runTest1
    -- print =<< (void $ eval $ parsertListParser_ab (genList_ab 100000))
    -- print =<< (void $ eval $ pureListParser_a (genList_a 1000000))
    -- print =<< (void $ eval $ parsertListParser_a       (genList_a 1000000))
    -- print =<< (void $ eval $ Strict.evalState mtlStateListParser_a_typed (genList_a 1000000))
    -- print =<< (void $ eval $ Strict.evalState mtlStrictStateListParser_a (genList_a 1000000))
    -- hSetBuffering stdout NoBuffering
    defaultMain
        [ bgroup "a*" $
            [ bgroup "pure"               $ expCodeGenBench genList_a pureListParser_a                              <$> [6..6]
            -- , bgroup "S2"               $ expCodeGenBench genList_a (S2.runState s2ListParser_a)                  <$> [6..6]
            , bgroup "mtl.State.Strict"   $ expCodeGenBench genList_a (Strict.evalState mtlStateListParser_a)       <$> [6..6]
            , bgroup "mtl.State.Strict'"  $ expCodeGenBench genList_a (Strict.evalState mtlStateListParser_a_typed)      <$> [6..6]
            , bgroup "mtl.State.Strict''" $ expCodeGenBench genList_a (Strict.evalState mtlStateListParser_a_let)     <$> [6..6]
            , bgroup "mtl.State.Lazy"     $ expCodeGenBench genList_a (Lazy.evalState   mtlStateListParser_a)       <$> [6..6]
            , bgroup "layered.State"      $ expCodeGenBench genList_a (evalState        layeredStateListParser_a)   <$> [6..6]
            ]
        -- , bgroup "mtl  (a*) parsing"    $ expCodeGenBench genList_a pureListParser_a       <$> [6..7]
        -- , bgroup "ab manual parsing" $ expCodeGenBench genList_ab pureListParser_ab <$> [4..5]
    --         -- [ bench "ab chain (10e6)"     $ env (expCodeGen genList_ab) (\s -> )
    --         -- , bench "1R trans (10e6)"     $ whnf (flip (evalState @Int) 0 . flip (evalStateT @String) "") (repeatM 1000000 incState)
    --         -- , bench "1L trans (10e6)"     $ whnf (flip (evalState @String) "" . flip (evalStateT @Int) 0) (repeatM 1000000 incState)
    --         -- , bench "2R trans (10e6)"     $ whnf (flip (evalState @Int) 0 . flip (evalStateT @String) "" . flip (evalStateT @Char) 'x') (repeatM 1000000 incState)
    --         -- , bench "2L trans (10e6)"     $ whnf (flip (evalState @Char) 'x' . flip (evalStateT @String) "" . flip (evalStateT @Int) 0) (repeatM 1000000 incState)
    --         -- , bench "3R trans (10e6)"     $ whnf (flip (evalState @Int) 0 . flip (evalStateT @String) "" . flip (evalStateT @Char) 'x' . flip (evalStateT @()) ()) (repeatM 1000000 incState)
    --         -- , bench "3L trans (10e6)"     $ whnf (flip (evalState @()) () . flip (evalStateT @Char) 'x' . flip (evalStateT @String) "" . flip (evalStateT @Int) 0) (repeatM 1000000 incState)
    --         -- ]
        ]
