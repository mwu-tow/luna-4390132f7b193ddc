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
-- hardcodedRepo :: Repo
-- hardcodedRepo = Repo defpkgs ["studio"] where
--     defpkgs = mempty & at "lib1"         .~ Just (Package "lib1 synopsis"   BatchApp $ fromList [ (Version 1 0 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc mempty "path")] )])
--                      & at "luna-studio"  .~ Just (Package "studio synopsis" GuiApp $ fromList [ (Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          , (Version 1 0 0 (Just $ RC 6), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          , (Version 1 1 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          ])








-- TODO: To refactor
instance Convertible FilePath Text where
    convert = convert . encodeString

run :: (MonadStates '[EnvConfig, RepoConfig, PackageConfig] m, MonadIO m, MonadException SomeException m, MonadSh m, MonadShControl m) => DevelopOpts -> m ()
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
    print appName
    print appPath
    repo <- getRepo
    resolvedApplication <- resolvePackageApp repo appName
    mapM_ (downloadAndUnpackDependency $ convert appPath) $ resolvedApplication ^. pkgsToPack
    --generate packageConfig.yaml
    generateYaml repo resolvedApplication (convert appPath) (convert appPath </> "luna-package.yaml")
    pkgConfig <- get @PackageConfig
    let versionFile = convert appPath </> (pkgConfig ^. configFolder) </> (pkgConfig ^. versionFileName)
    Shelly.mkdir_p $ parent versionFile
    liftIO $ writeFile (encodeString versionFile) "develop"


    -- building backend
    -- putStrLn "Building Luna Studio Backend"
    -- Shelly.chdir (appPath </> "build" </> "backend") $ do
    --     Shelly.run stackBin ["build", "--copy-bins", "--fast", "--install-ghc", appPath]

    return ()



-- - luna-workspace
--   - apps
--     - luna
--     - luna-studio
--   - tools
--     - stack
--
