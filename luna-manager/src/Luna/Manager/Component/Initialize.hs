module Luna.Manager.Component.Initialize where

import Prologue hiding (FilePath)
import qualified Data.Map as Map
import Luna.Manager.Component.Repository
import Luna.Manager.Component.Version
import Luna.Manager.System.Host

-- === Definition === --

data Initialize = Initialize { _application :: [Apps]} deriving (Show, Generic, Eq)
data Apps = Apps { _appName     :: Text
                 , _appVersions :: [Version]} deriving (Show, Generic, Eq)

makeLenses ''Initialize
makeLenses ''Apps

getVersionsList :: Repo -> Text -> [Version]
getVersionsList repo appName = sort . Map.keys $ vmap  where
    appPkg = fromMaybe (error "no package for this app") $ Map.lookup appName $ repo ^. packages
    vmap   = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions

resolveAppToInitialize :: Repo -> Text -> Apps
resolveAppToInitialize repo name = Apps name versions where
    versions = getVersionsList repo name

-- generateInitialJSON :: Repo -> Initialize
-- generateInitialJSON repo = do
--     -- apps <- repo ^. apps
--     resolved <- map (resolveAppToInitialize repo)  (repo ^. apps)
--     return $ Initialize resolved
