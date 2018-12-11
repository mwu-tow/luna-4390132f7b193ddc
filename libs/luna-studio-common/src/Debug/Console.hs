{-# LANGUAGE CPP #-}
module Debug.Console where

import qualified Prologue as P
import           Prologue hiding (putStrLn)
#ifdef __GHCJS__
import           Data.JSString (JSString, pack)
#endif


print :: (MonadIO m, Show a) => a -> m ()
putStrLn :: MonadIO m => String -> m ()
printLn :: MonadIO m => m ()

#ifdef __GHCJS__
printLn = putStrLn def
foreign import javascript safe "console.log($1)" consoleLog :: JSString -> IO ()
putStrLn = liftIO . consoleLog . pack
print = putStrLn . show
#else
putStrLn = P.putStrLn
print = P.print
printLn = putStrLn def
#endif
