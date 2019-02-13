module Main where

import Prologue
import Text.Parsert hiding ((.))
import Control.Monad.State.Layered hiding ((.))
import qualified Data.Text as Text

satisfy2' :: (Char -> Bool) -> StateT Int (StateT (StreamX Text) (FailParser String Identity)) Char
satisfy2' f = get @StreamX >>= \(StreamX !s) -> case Text.uncons s of
    Just (!t, !s') -> if f t then put @StreamX (StreamX s') >> setProgress >> return t else raise SatisfyError
    Nothing        -> raise EmptyStreamError
{-# INLINE satisfy2' #-}

satisfyStreamTextParser_a' :: StateT Int (StateT (StreamX Text) (FailParser String Identity)) Bool
satisfyStreamTextParser_a' = go where
    go = (satisfy2' ('a' ==) >> go) <|> (pure False)
{-# INLINE satisfyStreamTextParser_a' #-}

main = return ()
