module Luna.Manager.Repository where

import Prologue

import Luna.Manager.Version
import Luna.Manager.System.Host
import Luna.Manager.System.Path
import Luna.Manager.System.Config
import Luna.Manager.Config.Class
import Luna.Manager.Config.Aeson
import Luna.Manager.Pretty
import Luna.Manager.Network

import Control.Monad.Raise
import Control.Monad.State.Layered
import Data.Map                      (Map)
import Data.Aeson                    (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Text           as Text
import qualified Data.Yaml           as Yaml
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON


------------------------
-- === Repository === --
------------------------

-- === Definition === --

data Repo        = Repo        { _apps     :: Map Text Package , _libs     :: Map Text Package, _defaultApp :: Text } deriving (Show, Generic, Eq)
data Package     = Package     { _synopsis :: Text             , _versions :: VersionMap                            } deriving (Show, Generic, Eq)
data PackageDesc = PackageDesc { _deps     :: [PackageDep]     , _path     :: URIPath                               } deriving (Show, Generic, Eq)
data PackageDep  = PackageDep  { _name     :: Text             , _version  :: Version                               } deriving (Show, Generic, Eq)
type VersionMap = Map Version (Map SysDesc PackageDesc)


-- === Instances === --

-- JSON
instance ToJSON   Repo        where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   Package     where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   PackageDesc where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   PackageDep  where toEncoding = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance FromJSON Repo        where parseJSON  = lensJSONParse
instance FromJSON Package     where parseJSON  = lensJSONParse
instance FromJSON PackageDesc where parseJSON  = lensJSONParse
instance FromJSON PackageDep  where parseJSON  = lensJSONParse

-- Lenses
makeLenses ''Repo
makeLenses ''Package
makeLenses ''PackageDesc
makeLenses ''PackageDep

-- Show
instance Pretty PackageDep where
    showPretty (PackageDep n v) = n <> "-" <> showPretty v
    readPretty t = mapLeft (const "Conversion error") $ PackageDep s <$> readPretty ss where
        (s,ss) = Text.breakOnEnd "-" t



-----------------------------------
-- === Repository management === --
-----------------------------------

-- === Definition === --

data RepoConfig = RepoConfig { _repoPath   :: URIPath
                             , _cachedRepo :: Maybe Repo
                             }
makeLenses ''RepoConfig


-- === Utils === --

type MonadRepo m = (MonadStates '[RepoConfig, SystemConfig] m, MonadNetwork m)

getRepo :: MonadRepo m => m Repo
getRepo = do
    cfg <- get @RepoConfig
    case cfg ^. cachedRepo of
        Just r  -> return r
        Nothing -> do
            setTmpCwd
            downloadedConfig <- downloadFromURL . view repoPath =<< get @RepoConfig
            repo <- tryRight' =<< liftIO (Yaml.decodeFileEither $ convert downloadedConfig)
            put @RepoConfig $ cfg & cachedRepo .~ Just repo
            return repo


-- === Instances === --

instance {-# OVERLAPPABLE #-} MonadIO m => MonadDefaultConfig RepoConfig sys m where
    defaultConfig = return $ RepoConfig { _repoPath   = "https://luna-lang.org/releases/config.yaml"
                                        , _cachedRepo = Nothing
                                        }
