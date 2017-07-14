module Unsafe.Debug where

import           Prologue
import           System.IO.Unsafe
import           Data.Time.Clock  (diffUTCTime, getCurrentTime)
import System.CPUTime
import Control.Concurrent

debugPrint :: (Show a, Monad m) => a -> m ()
debugPrint = unsafeLiftIO . print

debugPPrint :: (Show a, Monad m) => a -> m ()
debugPPrint = unsafeLiftIO . pprint

debugPutStrLn :: Monad m => String -> m ()
debugPutStrLn = unsafeLiftIO . putStrLn

unsafeLiftIO :: Monad m => IO a -> m a
unsafeLiftIO action = do
    let r = unsafePerformIO action
    r `seq` return r

timeIt' :: Monad m => String -> m a -> m a
timeIt' name action = do
    (cpuStart, start) <- unsafeLiftIO $ do
        putStrLn $ ">> " <> name
        (,) <$> getCPUTime <*> getCurrentTime
    r <- action

    r `seq` unsafeLiftIO $ do
        cpuEnd <- getCPUTime
        end    <- getCurrentTime
        putStrLn $ "<< " <> name
                 <> " CPU " <> show ((cpuEnd - cpuStart) `div` 1000000000) <> "ms"
                 <> " Wall " <> show (diffUTCTime end start)
    return r


timeIt :: MonadIO m => String -> m a -> m a
timeIt name action = do
    putStrLn $ ">> " <> name
    cpuStart <- liftIO getCPUTime
    start    <- liftIO getCurrentTime
    r <- action
    cpuEnd <- liftIO getCPUTime
    end    <- liftIO getCurrentTime
    putStrLn $ "<< " <> name
            <> " CPU " <> show ((cpuEnd - cpuStart) `div` 1000000000) <> "ms"
            <> " Wall " <> show (diffUTCTime end start)
    return r
