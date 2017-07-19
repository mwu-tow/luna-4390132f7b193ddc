module Luna.Manager.Component.Repository where

import Prologue

import Luna.Manager.Component.Version
import Luna.Manager.System.Host
import Luna.Manager.System.Path
import Luna.Manager.System.Env
import Luna.Manager.Component.Pretty
import Luna.Manager.Network

import Control.Lens.Aeson
import Control.Monad.Raise
import Control.Monad.State.Layered
import Data.Map                      (Map)
import Data.Aeson                    (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON)
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Yaml           as Yaml
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import Filesystem.Path.CurrentOS (encodeString)

------------------------
-- === Repository === --
------------------------
-- FIXME: Features for luna-manager 1.1:
--        - We should also keep sha of every package to be sure we downloaded valid one.
--        - We should keep sha of whole yaml and keep it separate on server, so yamls could be cached locally and we can check if they are up to date with VERY low bandwich

-- === Definition === --
data AppType = BatchApp | GuiApp | Lib deriving (Show, Generic, Eq)


-- Core
data Repo          = Repo          { _packages :: Map Text Package , _apps     :: [Text]                          } deriving (Show, Generic, Eq)
data Package       = Package       { _synopsis :: Text             , _versions :: VersionMap, _appType :: AppType } deriving (Show, Generic, Eq)
data PackageDesc   = PackageDesc   { _deps     :: [PackageHeader]  , _path     :: Text                         } deriving (Show, Generic, Eq)
data PackageHeader = PackageHeader { _name     :: Text             , _version  :: Version                         } deriving (Show, Generic, Eq)
type VersionMap    = Map Version (Map SysDesc PackageDesc)

-- Helpers
data ResolvedPackage = ResolvedPackage { _header :: PackageHeader, _desc :: PackageDesc, _resolvedAppType :: AppType } deriving (Show, Generic, Eq)

makeLenses ''Repo
makeLenses ''Package
makeLenses ''PackageDesc
makeLenses ''PackageHeader
makeLenses ''ResolvedPackage

-- === Utils === --

lookupPackage :: Repo -> PackageHeader -> Maybe ResolvedPackage
lookupPackage repo h = do
    des <- repo ^? packages . ix (h ^. name) . versions . ix (h ^. version) . ix currentSysDesc
    apptype <- repo ^? packages . ix (h ^. name) . appType
    return $ ResolvedPackage h des apptype

resolveSingleLevel :: Repo -> PackageDesc -> ([PackageHeader], [ResolvedPackage])
resolveSingleLevel repo desc = partitionEithers $ zipWith combine directSubDeps directSubPkgs where
    directSubDeps  = desc ^. deps
    directSubPkgs  = lookupPackage repo <$> directSubDeps
    combine h      = maybe (Left h) Right

resolve :: Repo -> PackageDesc -> ([PackageHeader], [ResolvedPackage])
resolve repo pkg = (errs <> subErrs, oks <> subOks) where
    (errs, oks) = resolveSingleLevel repo pkg
    subDescs    = view desc <$> oks
    subRes      = resolve repo <$> subDescs
    subErrs     = concat $ fst <$> subRes
    subOks      = concat $ snd <$> subRes


-- === Instances === --

-- JSON
instance ToJSON   AppType        where toEncoding = lensJSONToEncoding . show; toJSON = lensJSONToJSON . show
instance ToJSON   Repo           where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   Package        where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   PackageDesc    where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   PackageHeader  where toEncoding = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance FromJSON AppType        where parseJSON  = lensJSONParse
instance FromJSON Repo           where parseJSON  = lensJSONParse
instance FromJSON Package        where parseJSON  = lensJSONParse
instance FromJSON PackageDesc    where parseJSON  = lensJSONParse
instance FromJSON PackageHeader  where parseJSON  = either (fail . convert) return . readPretty <=< parseJSON

-- Show
instance Pretty PackageHeader where
    showPretty (PackageHeader n v) = n <> "-" <> showPretty v
    readPretty t = mapLeft (const "Conversion error") $ PackageHeader s <$> readPretty ss where
        (s,ss) = Text.breakOnEnd "-" t & _1 %~ Text.init



-----------------------------------
-- === Repository management === --
-----------------------------------

-- === Definition === --

data RepoConfig = RepoConfig { _repoPath   :: URIPath
                             , _cachedRepo :: Maybe Repo
                             }
makeLenses ''RepoConfig


-- === Utils === --

type MonadRepo m = (MonadStates '[RepoConfig, EnvConfig] m, MonadNetwork m)

parseConfig :: (MonadIO m, MonadException SomeException m) => FilePath -> m Repo
parseConfig cfgPath =  tryRight' =<< liftIO (Yaml.decodeFileEither cfgPath)

getRepo :: MonadRepo m => m Repo
getRepo = do
    cfg <- get @RepoConfig
    case cfg ^. cachedRepo of
        Just r  -> return r
        Nothing -> do
            setTmpCwd
            downloadedConfig <- downloadFromURL . view repoPath =<< get @RepoConfig
            repo <- parseConfig $ encodeString downloadedConfig
            put @RepoConfig $ cfg & cachedRepo .~ Just repo
            return repo


-- === Instances === --

instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig RepoConfig sys arch m where
    defaultHostConfig = return $ RepoConfig { _repoPath   = "https://s3-us-west-2.amazonaws.com/packages-luna/config.yaml"
                                            , _cachedRepo = Nothing
                                            }
