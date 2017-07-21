{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Main where

import Prologue as P
import Criterion.Main
import Control.Monad.State.Layered hiding ((.))
import qualified Control.Monad.State.Strict as S
import Control.Monad.Codensity
import qualified Control.Monad.State.CPS as CPS
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Random
import Text.Parsert hiding ((.))


eval :: NFData a => a -> IO a
eval = evaluate . force


liftExp :: (Int -> a) -> (Int -> a)
liftExp f = f . (10^)

expCodeGen :: NFData a => (Int -> a) -> (Int -> IO a)
expCodeGen f i = do
    putStrLn $ "generating input code (10e" <> show i <> " chars)"
    out <- eval $ liftExp f i
    putStrLn "code generated sucessfully"
    return out
{-# INLINE expCodeGen #-}

maxExpCodeLen :: Int
maxExpCodeLen = 8

expCodeGenBench :: (NFData a, NFData b) => (Int -> a) -> (a -> b) -> Int -> Benchmark
expCodeGenBench f p i = env (expCodeGen f i) $ bench ("10e" <> show i) . nf p ; {-# INLINE expCodeGenBench #-}

genChain_ab :: Int -> [Char]
genChain_ab i = take i $ randomRs ('a', 'b') (mkStdGen 0) ; {-# INLINE genChain_ab #-}

-- (a|b)*
manualChainParser_ab :: [Char] -> Bool
manualChainParser_ab = \case
    'a':s -> manualChainParser_ab s
    'b':s -> manualChainParser_ab s
    []    -> True
    _     -> False
{-# INLINE manualChainParser_ab #-}

chainParser_ab :: [Char] -> Either (NonEmpty String) [Char]
chainParser_ab s = runIdentity
                --  $ evalBacktracker
                 $ runFailParser
                 $ evalStreamProvider (listStream s)
                --  $ evalOffsetRegister
                 $ (many (token 'a' <|> token 'b') )



main = do
    print $ chainParser_ab (genChain_ab 10000)
    hSetBuffering stdout NoBuffering
    defaultMain
        [ bgroup "ab parsing"        $ expCodeGenBench genChain_ab chainParser_ab       <$> [4..5]
        , bgroup "ab manual parsing" $ expCodeGenBench genChain_ab manualChainParser_ab <$> [4..5]
            -- [ bench "ab chain (10e6)"     $ env (expCodeGen genChain_ab) (\s -> )
            -- , bench "1R trans (10e6)"     $ whnf (flip (evalState @Int) 0 . flip (evalStateT @String) "") (repeatM 1000000 incState)
            -- , bench "1L trans (10e6)"     $ whnf (flip (evalState @String) "" . flip (evalStateT @Int) 0) (repeatM 1000000 incState)
            -- , bench "2R trans (10e6)"     $ whnf (flip (evalState @Int) 0 . flip (evalStateT @String) "" . flip (evalStateT @Char) 'x') (repeatM 1000000 incState)
            -- , bench "2L trans (10e6)"     $ whnf (flip (evalState @Char) 'x' . flip (evalStateT @String) "" . flip (evalStateT @Int) 0) (repeatM 1000000 incState)
            -- , bench "3R trans (10e6)"     $ whnf (flip (evalState @Int) 0 . flip (evalStateT @String) "" . flip (evalStateT @Char) 'x' . flip (evalStateT @()) ()) (repeatM 1000000 incState)
            -- , bench "3L trans (10e6)"     $ whnf (flip (evalState @()) () . flip (evalStateT @Char) 'x' . flip (evalStateT @String) "" . flip (evalStateT @Int) 0) (repeatM 1000000 incState)
            -- ]
        ]
