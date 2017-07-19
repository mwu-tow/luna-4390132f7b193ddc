{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where



import           Control.Lens.Aeson
import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, decodeString, parent)
import           Luna.Manager.Archive
import           Luna.Manager.Command.Options (MakePackageOpts)
import           Luna.Manager.Component.Repository as Repo
import           Luna.Manager.Network
import           Luna.Manager.System (makeExecutable)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path
import           Luna.Manager.Component.Version (Version)
import           Luna.Manager.Component.Pretty
import           Prologue hiding (FilePath)
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import qualified Luna.Manager.Command.Options as Opts
import qualified Shelly.Lifted as Shelly
import qualified System.Process.Typed as Process



data PackageConfig = PackageConfig { _defaultPackagePath     :: FilePath
                                   , _studioName             :: Text -- x
                                   , _lunaName               :: Text -- x
                                   , _backendBuildPath       :: FilePath -- x
                                   , _frontendBuildPath      :: FilePath -- x
                                   , _lunaBuildPath          :: FilePath -- x
                                   , _atomPrepareScriptPath  :: FilePath
                                   , _studioComponentsToCopy :: [FilePath]
                                   , _lunaComponentsToCopy   :: [FilePath]
                                   , _studioFolderName       :: Text
                                   , _studioUtilsFolder      :: Text
                                   , _lunaUtilsFolder        :: Text
                                   , _logoFileName           :: Text
                                   , _desktopFileName        :: Text
                                   }

makeLenses ''PackageConfig


instance Monad m => MonadHostConfig PackageConfig 'Linux arch m where
    defaultHostConfig = return $ PackageConfig
        { _defaultPackagePath = "~/luna-package"
        , _studioName = "luna-studio"
        , _lunaName   = "luna"
        , _backendBuildPath = "./build/backend"
        , _frontendBuildPath = "./luna-studio"
        , _lunaBuildPath = "./shell"
        , _atomPrepareScriptPath = "./luna-studio/script/atom_prepare.py"
        , _studioComponentsToCopy = ["dist/bin", "env", "supervisor", "luna-studio/atom", "resources"]
        , _lunaComponentsToCopy = ["./shell/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/bin/luna"]
        , _studioFolderName = "luna"
        , _studioUtilsFolder = "resources"
        , _lunaUtilsFolder = "docs"
        , _logoFileName = "logo.png"
        , _desktopFileName = "app.desktop"
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & studioComponentsToCopy .~ ["dist/bin", "env", "supervisor", "luna-studio/atom"]

-- Map Name ResolvedPackage
data ResolvedPackageMap  = ResolvedPackageMap { _appName    :: Text
                                              , _appVersion :: Version
                                            --   , _header :: Header
                                              , _appDesc    :: PackageDesc
                                              , _pkgsToPack :: [ResolvedPackage] -- jak zunifikowac to z typami z Repository?
                                              } deriving (Show)
makeLenses ''ResolvedPackageMap

type MonadCreatePackage m = (MonadStates '[EnvConfig, PackageConfig] m, MonadNetwork m)









build :: Shelly.MonadSh m => FilePath -> m ()
build path = do
    Shelly.cd path
    Shelly.cmd "stack" "build"

buildCopyBins :: Shelly.MonadSh m => FilePath -> m ()
buildCopyBins path = do
    Shelly.cd path
    Shelly.cmd "stack" "build" "--copy-bins"

prepareAtomPkg :: MonadCreatePackage m => FilePath -> m ()
prepareAtomPkg path = do
    pkgConfig <- get @PackageConfig
    Shelly.shelly $ do
        Shelly.cmd $  path </> (pkgConfig ^. atomPrepareScriptPath)

runStackBuild :: MonadCreatePackage m => Text -> FilePath -> m () -- wypadna informacje o pakiecie bo bedzie jeden skrypt na repo do stack builda
runStackBuild appName repoPath = do
    pkgConfig <- get @PackageConfig
    let studioAppName = pkgConfig ^. studioName
        lunaAppName = pkgConfig ^. lunaName
    case appName of
        a | a == studioAppName -> do
            let backendAbsolutePath  = repoPath </> (pkgConfig ^. backendBuildPath)
                frontendAbsolutePath = repoPath </> (pkgConfig ^. frontendBuildPath)
            Shelly.shelly $ buildCopyBins backendAbsolutePath
            Shelly.shelly $ build frontendAbsolutePath
            prepareAtomPkg repoPath
          | a == lunaAppName -> do
            Shelly.shelly $ build lunaBuildAbsolutePath where
            lunaBuildAbsolutePath = repoPath </> (pkgConfig ^. lunaBuildPath)


copyFromRepository :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromRepository appName repoPath = do
    pkgConfig <- get @PackageConfig

    let studioAppName = pkgConfig ^. studioName
        lunaAppName = pkgConfig ^. lunaName

    packageRepoFolder <- if appName == studioAppName
        then expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> convert (pkgConfig ^. studioFolderName)
        else if appName == lunaAppName
            then expand $ (pkgConfig ^. defaultPackagePath) </> convert appName
            else error "no such app to make package"
    Shelly.shelly $ Shelly.mkdir_p packageRepoFolder
    case appName of
        a | a == studioAppName -> do
            let expandedCopmponents = (repoPath </> ) <$> (pkgConfig ^. studioComponentsToCopy)
            Shelly.shelly $ (flip Shelly.cp_r packageRepoFolder) `mapM_` expandedCopmponents
          | a == lunaAppName -> do
            let expandedCopmponents = (repoPath </> ) <$> (pkgConfig ^. lunaComponentsToCopy)
            Shelly.shelly $ (flip Shelly.cp_r packageRepoFolder) `mapM_` expandedCopmponents

downloadAndUnpackDependency :: MonadCreatePackage m => Text -> ResolvedPackage -> m ()
downloadAndUnpackDependency appName resolvedPackage = do
    pkgConfig <- get @PackageConfig
    let depName = resolvedPackage ^. header . name
    pkgFolderPath <- expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> convert depName
    atomAppPath <- expand $ (pkgConfig ^. defaultPackagePath) </> convert appName
    downloadedPkg <- downloadFromURL $ resolvedPackage ^. desc . path
    unpacked <- unpackArchive downloadedPkg
    case currentHost of
        Linux -> Shelly.shelly $ Shelly.cmd "mv" unpacked pkgFolderPath
        Darwin -> case depName of
            "atom" -> Shelly.shelly $ Shelly.cp_r unpacked atomAppPath --TODO moze atompowinien byc oznaczony jako gui app i inaczej kopiowany?
            otherwise -> Shelly.shelly $ Shelly.cmd "mv" unpacked pkgFolderPath

runApm :: (MonadIO m, Shelly.MonadSh m) => FilePath -> FilePath -> FilePath -> m ()
runApm apmPath atomHomePath onigurumaPath = do
    print $ show apmPath
    print $ show atomHomePath
    Shelly.cd atomHomePath
    let nodeModulesAtomHome = atomHomePath </> "node_modules"
        onigurumaAtomHome = atomHomePath </> "node_modules" </> "oniguruma"
    Shelly.mkdir_p nodeModulesAtomHome
    Shelly.cp_r onigurumaPath nodeModulesAtomHome
    Shelly.setenv "ATOM_HOME" $ Shelly.toTextIgnore atomHomePath
    Shelly.cmd apmPath "install" "."


createAppimage :: MonadCreatePackage m => Text -> m ()
createAppimage appName = do
    pkgConfig <- get @PackageConfig
    tmpAppDirPath <- expand $ (pkgConfig ^. defaultPackagePath) </> "appimage" </> convert appName </> convert (appName <> ".AppDir")
    tmpAppPath <- expand $ (pkgConfig ^. defaultPackagePath) </> "appimage" </> convert appName

    srcPkgPath <- (expand $ (pkgConfig ^. defaultPackagePath) </> convert appName)
    srcLibPath <- (expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> "zmq" </> "lib64")
    let dstPkgPath = (tmpAppDirPath </> "usr" </> "bin")
        dstLibPath = (tmpAppDirPath </> "usr" </> "lib")

    Shelly.shelly $ Shelly.mkdir_p tmpAppDirPath
    Shelly.shelly $ Shelly.cd tmpAppPath
    functions <- downloadWithProgressBar "https://github.com/probonopd/AppImages/raw/master/functions.sh" tmpAppPath
    Shelly.shelly $ do
        Shelly.mkdir_p $ tmpAppDirPath </> "usr" </> "bin"
        Shelly.mkdir_p $ tmpAppDirPath </> "usr" </> "lib"

    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "get_apprun"

    logoFile <- expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> convert (pkgConfig ^. studioFolderName) </> convert (pkgConfig ^. studioUtilsFolder) </> convert (pkgConfig ^. logoFileName)
    desktopFile <- expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> convert (pkgConfig ^. studioFolderName) </> convert (pkgConfig ^. studioUtilsFolder) </> convert (pkgConfig ^. desktopFileName)

    Shelly.shelly $ do
        Shelly.cp logoFile $ tmpAppDirPath </> convert (appName <> ".png")
        Shelly.cp desktopFile $ tmpAppDirPath </> convert (appName <> ".desktop")
        copyDir srcPkgPath dstPkgPath
        copyDir srcLibPath dstLibPath
    appWrapper <- downloadWithProgressBar "https://raw.githubusercontent.com/probonopd/AppImageKit/master/desktopintegration" tmpAppDirPath
    let dstWrapper = (dstPkgPath </> convert (appName <> ".wrapper"))
    Shelly.shelly $ Shelly.mv appWrapper dstWrapper
    makeExecutable dstWrapper
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ "sed -i -e \"s|Exec=" ++ (convert appName) ++ "|Exec=" ++ (convert appName) ++".wrapper|g\" " ++ (convert appName) ++ ".desktop"
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppPath) $ Process.setEnv [("APP", (convert appName))] $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "generate_type2_appimage"

apmPath :: MonadCreatePackage m => FilePath -> Text -> m FilePath
apmPath defaultPackagePath appName = case currentHost of
    Linux  -> expand $ defaultPackagePath </> convert appName </> "atom" </> "usr" </> "share" </> "atom" </> "resources" </> "app" </> "apm" </> "bin" </> "apm"
    Darwin -> expand $ defaultPackagePath </> convert appName </> "Atom.app" </> "Contents" </> "Resources" </> "app" </> "apm" </> "bin" </> "apm"

onigurumaPath :: MonadCreatePackage m => FilePath -> Text -> m FilePath
onigurumaPath defaultPackagePath appName = case currentHost of
    Linux  -> expand $ defaultPackagePath </> convert appName </> "atom" </> "usr" </> "share" </> "atom" </> "resources" </> "app" </> "node_modules" </> "oniguruma"
    Darwin -> expand $ defaultPackagePath </> convert appName </> "Atom.app" </> "Contents" </> "Resources" </> "app" </> "node_modules" </> "oniguruma"

linkLibs :: MonadIO m => FilePath -> m ()
linkLibs binPath = do

    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./broker"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./bus-logger"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./luna-empire-invoker"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./luna-empire-logger"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./luna-empire"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./request-monitor"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./undo-redo"
    Process.runProcess_ $ Process.setWorkingDir (encodeString binPath) $ Process.shell "install_name_tool -change /usr/local/opt/zeromq/lib/libzmq.5.dylib @executable_path/`basename /usr/local/opt/zeromq/lib/libzmq.5.dylib` ./ws-connector"


createPkg :: MonadCreatePackage m => ResolvedPackageMap -> m ()
createPkg resolvedApp = do
    pkgConfig <- get @PackageConfig
    runStackBuild (resolvedApp ^. appName) $ convert (resolvedApp ^. appDesc . path) -- convert -> convert
    copyFromRepository (resolvedApp ^. appName) $ convert (resolvedApp ^. appDesc . path)
    mapM_ (downloadAndUnpackDependency (resolvedApp ^. appName)) (resolvedApp ^. pkgsToPack)
    let studio = pkgConfig ^. studioName
        luna = pkgConfig ^. lunaName
    case (resolvedApp ^. appName) of
        a | a == studio -> do
            apm             <- apmPath (pkgConfig ^. defaultPackagePath) (resolvedApp ^. appName)
            oniguruma       <- onigurumaPath (pkgConfig ^. defaultPackagePath) (resolvedApp ^. appName)
            atomHomePath    <- expand $ (pkgConfig ^. defaultPackagePath) </> convert (resolvedApp ^. appName) </> "luna-atom" </> "packages" </> convert (resolvedApp ^. appName)
            atomDirInStudio <- expand $ (pkgConfig ^. defaultPackagePath) </> convert (resolvedApp ^. appName) </> convert (pkgConfig ^. studioFolderName) </> "atom"
            Shelly.shelly $ Shelly.mkdir_p atomHomePath
            Shelly.shelly $ copyDir atomDirInStudio atomHomePath
            Shelly.shelly $ runApm apm atomHomePath oniguruma
          | a == luna -> return ()
    mainAppDir <- expand $ (pkgConfig ^. defaultPackagePath) </> convert (resolvedApp ^. appName) </> convert (resolvedApp ^. appName)
    case (resolvedApp ^. appName) of
        a | a == studio -> do
            Shelly.shelly $ Shelly.cp "./executables/luna-studio-runner"  mainAppDir
            print $ show mainAppDir
            let versionFile = (parent mainAppDir) </>  "version.txt"
            liftIO $ writeFile (encodeString versionFile) $ convert $ showPretty (resolvedApp ^. appVersion)
          | a == luna -> return ()
    case currentHost of
        Linux  -> createAppimage (resolvedApp ^. appName)
        Darwin -> do
            case (resolvedApp ^. appName) of
                a | a == studio -> do
                    Shelly.shelly $ Shelly.cp ((parent mainAppDir) </> "zmq" </> "libzmq.5.dylib") ((parent mainAppDir) </> "luna" </> "bin")
                    Shelly.shelly $ linkLibs $ (parent mainAppDir) </> "luna" </> "bin"
                  | a == luna -> return ()
            Shelly.shelly $ createTarGzUnix (parent mainAppDir) (resolvedApp ^. appName)
            return ()



runCreatingPackage :: MonadCreatePackage m => MakePackageOpts -> m ()
runCreatingPackage opts = do
    repo <- parseConfig $ convert (opts ^. Opts.cfgPath)
    --TODO separate function for resolving package
    let appsToPack = repo ^. apps
        appPkg  = map (\app -> (app, Map.lookup app (repo ^. packages))) appsToPack
        pkgList = map (\(name, package) -> (name, fromMaybe (error $ convert $ "package undefined in yaml file: " <> name) package)) appPkg  -- TODO zrób to bezpiecznie!
        pkgMap  = map (\(name, package) -> (name, fst $ head $ toList $ package ^. versions, fromMaybe (error $ "no package description") $ Map.lookup currentSysDesc $ snd $ head $ toList $ package ^. versions) ) pkgList  -- TODO zrób to bezpiecznie!
        resolvedMap = map (\(name, version, pkgDesc) -> ResolvedPackageMap name version pkgDesc $ snd $ resolve repo pkgDesc) pkgMap -- TODO zrób to bezpiecznie!

    mapM_ createPkg resolvedMap


    return ()
