{-# LANGUAGE OverloadedStrings #-}
module JS.LocalStorage
    ( saveLocation
    , loadLocation
    , saveCamera
    , loadCamera
    , saveVisualizationPreferences
    , loadVisualizationPreferences
    ) where

import           Common.Prelude
import           Data.Aeson                           (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy.Char8           as ByteString
import           Data.Hashable                        (hash)
import           Data.HashMap.Lazy                    (HashMap)
import           JavaScript.Web.Storage               (getItem, localStorage, setItem)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.GraphLocation        (GraphLocation, breadcrumb, filePath)
import           LunaStudio.Data.NodeValue            (Visualizer)
import           LunaStudio.Data.TypeRep              (TypeRep)


locationKey :: JSString
locationKey = "lastLocation"

cameraKey :: GraphLocation -> JSString
cameraKey gl = "camera:" <> convert (gl ^. filePath) <> "-" <> jsShow (hash $ gl ^. breadcrumb)

visualizationPreferencesKey :: JSString
visualizationPreferencesKey = "visualizations"

saveLocation :: GraphLocation -> IO ()
saveLocation = storageSave locationKey

loadLocation :: IO (Maybe GraphLocation)
loadLocation = storageLoad locationKey

saveCamera :: GraphLocation -> CameraTransformation -> IO ()
saveCamera gl camera = storageSave (cameraKey gl) camera

loadCamera :: GraphLocation -> IO (Maybe CameraTransformation)
loadCamera = storageLoad . cameraKey

saveVisualizationPreferences :: HashMap TypeRep Visualizer -> IO ()
saveVisualizationPreferences visMap = storageSave visualizationPreferencesKey visMap

loadVisualizationPreferences :: IO (Maybe (HashMap TypeRep Visualizer))
loadVisualizationPreferences = storageLoad visualizationPreferencesKey


storageSave :: ToJSON value => JSString -> value -> IO ()
storageSave key value = do
    let payload = convert . ByteString.unpack $ encode value
    setItem key payload localStorage

storageLoad :: FromJSON value => JSString -> IO (Maybe value)
storageLoad key = do
    payload <- getItem key localStorage
    return $ decode . ByteString.pack . convert =<< payload
