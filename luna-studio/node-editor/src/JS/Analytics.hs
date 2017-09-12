{-# LANGUAGE OverloadedStrings #-}

module JS.Analytics (
  Event(..)
, track
) where

import           Common.Prelude


data Event = AppStarted deriving (Show)

foreign import javascript safe "analytics.track($1)" track' :: JSString -> IO ()

track :: MonadIO m => Event -> m ()
track event = liftIO $ track' $ convert $ show event
