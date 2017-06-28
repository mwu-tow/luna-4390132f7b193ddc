module Unsafe.Debug where

import System.IO.Unsafe
import Prologue


debugPrint :: (Show a, Monad m) => a -> m ()
debugPrint = unsafeLiftIO . print

debugPPrint :: (Show a, Monad m) => a -> m ()
debugPPrint = unsafeLiftIO . pprint

unsafeLiftIO :: Monad m => IO a -> m a
unsafeLiftIO action = do
    let r = unsafePerformIO action
    r `seq` return r
