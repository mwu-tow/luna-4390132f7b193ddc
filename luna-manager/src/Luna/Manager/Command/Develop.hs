module Luna.Manager.Command.Develop where

import Prologue hiding (FilePath)
import Luna.Manager.Command.Options
import Luna.Manager.Network
import Luna.Manager.System.Env
import Control.Monad.Raise
import Control.Monad.State.Layered
import Luna.Manager.Shell.Shelly   (MonadSh, MonadShControl)
import Filesystem.Path.CurrentOS   (FilePath, (</>), encodeString, decodeString, toText, basename, hasExtension, parent)

import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified Luna.Manager.Archive      as Archive

import Luna.Manager.Component.Repository
import Luna.Manager.Command.CreatePackage --(downloadAndUnpackDependency, PackageConfig)
import Luna.Manager.Component.Version
import Luna.Manager.System.Host

import Control.Monad.Trans.Resource ( MonadBaseControl)

-- hardcodedRepo :: Repo
-- hardcodedRepo = Repo defpkgs ["studio"] where
--     defpkgs = mempty & at "lib1"         .~ Just (Package "lib1 synopsis"   BatchApp $ fromList [ (Version 1 0 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc mempty "path")] )])
--                      & at "luna-studio"  .~ Just (Package "studio synopsis" GuiApp $ fromList [ (Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          , (Version 1 0 0 (Just $ RC 6), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          , (Version 1 1 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          ])



data DevelopConfig = DevelopConfig { _stackPath      :: Text
                                   , _devPath        :: FilePath
                                   , _appsPath       :: FilePath
                                   , _toolsPath      :: FilePath
                                   , _stackLocalPath :: FilePath
                                    }
makeLenses ''DevelopConfig

type MonadDevelop m = (MonadStates '[EnvConfig, RepoConfig, PackageConfig, DevelopConfig] m, MonadIO m, MonadException SomeException m, MonadSh m, MonadShControl m, MonadCatch m, MonadBaseControl IO m)


instance Monad m => MonadHostConfig DevelopConfig 'Linux arch m where
    defaultHostConfig = return $ DevelopConfig
        { _stackPath      = "https://www.stackage.org/stack/linux-x86_64-static"
        , _devPath        = "luna-workspace"
        , _appsPath       = "apps"
        , _toolsPath      = "tools"
        , _stackLocalPath = "stack"
        }

instance Monad m => MonadHostConfig DevelopConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & stackPath .~ "https://www.stackage.org/stack/osx-x86_64"

instance Monad m => MonadHostConfig DevelopConfig 'Windows arch m where
    defaultHostConfig = defaultHostConfigFor @Linux


downloadAndUnpackStack :: MonadDevelop m => FilePath -> m ()
downloadAndUnpackStack path = do
    developConfig <- get @DevelopConfig
    let stackURL = developConfig ^. stackPath
        guiInstaller = False
        totalProgress = 1.0
        progressFielsdName = ""
    stackArch <- downloadWithProgressBar stackURL guiInstaller
    stackArch' <- Archive.unpack guiInstaller totalProgress progressFielsdName stackArch
    Shelly.mv stackArch' path
    return ()

cloneRepo :: MonadDevelop m => Text -> FilePath -> m Text
cloneRepo appName appPath = Shelly.run "git" ["clone", repoPath, Shelly.toTextIgnore appPath] where
    repoPath = "git@github.com:luna/" <> appName <> ".git"



run :: MonadDevelop m => DevelopOpts -> m ()
run opts = do
    -- root <- Shelly.pwd
    -- let devPath   = root      </> "luna-workspace"
    --     toolsPath = devPath   </> "tools"
    --     appsPath  = devPath   </> "apps"
    --     stackPath = toolsPath </> "stack"
    --     stackBin  = stackPath </> "stack"
    -- Shelly.mkdir_p toolsPath
    -- Shelly.mkdir_p appsPath

    -- Stack installation
    -- let stackName    = "stack"
    --     stackVersion = "1.5.1"
    -- putStrLn . convert $ "Downloading " <> stackName <> " (" <> stackVersion <> ")"
    -- stackArch  <- downloadWithProgressBar $ "https://github.com/commercialhaskell/stack/releases/download/v" <> stackVersion <> "/stack-" <> stackVersion <> "-linux-x86_64-static.tar.gz"
    -- stackArch' <- Archive.unpack stackArch
    -- Shelly.mv stackArch' stackPath

    -- cloning repo
    -- let appName  = "luna-studio"
    --     repoPath = "git@github.com:luna/" <> convert appName <> ".git"
    --     appPath  = appsPath </> appName
    -- putStrLn . convert $ "Clonning repository " <> repoPath
    -- Shelly.run "git" ["clone", repoPath, convert appPath]

    --downloading and installing dependencies
    let appName  = opts ^. target -- "luna-studio"
        appPath  = opts ^. repositoryPath
    repo <- getRepo
    resolvedApplication <- resolvePackageApp repo appName
    mapM_ (downloadAndUnpackDependency $ convert appPath) $ resolvedApplication ^. pkgsToPack
    --generate packageConfig.yaml
    generateYaml repo resolvedApplication (convert appPath </> "luna-package.yaml")
    pkgConfig <- get @PackageConfig




    return ()



-- - luna-workspace
--   - apps
--     - luna
--     - luna-studio
--   - tools
--     - stack
--
