{-# LANGUAGE OverloadedStrings #-}

module Common.Analytics  where

import           Common.Prelude
import           Data.Aeson     (Value, toJSON)
import           GHCJS.Marshal  (toJSVal_aeson)
import qualified JS.Analytics   as JS


class Show a => IsTrackedEvent a where
    eventName :: a -> Maybe String
    eventName = Just . head . words . show
    eventData :: a -> Value
    eventData = const $ toJSON (Nothing :: Maybe ())

track :: (MonadIO m, IsTrackedEvent event) => event-> m ()
track event = withJust (eventName event) $ \name ->
    liftIO $ JS.track (convert name) =<< toJSVal_aeson (eventData event)


data Event = AppStarted deriving (Show)

instance IsTrackedEvent Event

a <.$> b = ((a <> ".") <>) <$> b
