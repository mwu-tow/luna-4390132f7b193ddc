{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Prologue hiding (FilePath)

import GHC.Generics
import Control.Applicative
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as ByteStringL
import Data.Conduit (ConduitM, await, yield, ($$+-),($=+))
import Data.Conduit.List (sinkNull)
import Data.List.Split
import Text.Printf   ( printf )
import Data.Ratio    ( (%) )
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types (hContentLength)
import qualified Network.URI as URI
import Options.Applicative
import Path (parent, parseAbsFile, toFilePath)
import Control.Concurrent hiding(yield)
import qualified System.IO as System

import System.Console.ANSI (clearLine, cursorUpLine)
import System.FilePath.Posix (normalise, takeExtension, makeRelative, pathSeparator)
import qualified System.Directory as System
import qualified System.Environment as Environment
import System.Process.Typed
import qualified Data.Text as Text
import Control.Monad.State.Layered
import Control.Monad.Raise
import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map  as Map
import qualified Data.Text as Text
-- default (Text.Text)

import Luna.Manager.System.Host
import Luna.Manager.System.Config
import Luna.Manager.Version
import Luna.Manager.Config.Aeson
import Luna.Manager.Installer
import Luna.Manager.Config.Class
import Luna.Manager.Repository
import Luna.Manager.Shell
import Luna.Manager.System.Path
import Luna.Manager.Network












--
--
-- failIfNothing :: Text.Text -> Maybe a -> a
-- failIfNothing err = \case
--     Nothing -> error $ Text.unpack err --printErr err >> closeApp
--     Just a  -> a
--

-- mkRelativePath :: MonadIO m => [Text.Text] -> m FilePath
-- mkRelativePath args = do
--     let strArgs = Text.unpack <$> args
--         folded = foldl (\acc x -> acc </> x) "" strArgs
--         a = normalise $ folded
--     return a








    -- handleAll :: Monad m => (SomeException -> m a) -> ExceptT' m a -> m a

main :: IO ()
main = do
    handleAll (print) $ evalDefConfigState @SystemConfig
                      $ evalDefConfigState @InstallConfig
                      $ evalDefConfigState @RepoConfig
                      $ runInstaller
    print "dziala"

    putStrLn $ convert $ Yaml.encode $ harcodedRepo

-- data    Repo        = Repo        { _apps   :: Map Text Package, _libs    :: Map Text Package } deriving (Show, Generic, Eq)
-- newtype Package     = Package     { _pkgMap :: PkgMap                                         } deriving (Show, Generic, Eq)
-- data    PackageDesc = PackageDesc { _deps   :: [PackageDep]    , _path    :: Path             } deriving (Show, Generic, Eq)
-- data    PackageDep  = PackageDep  { _name   :: Text            , _version :: Version          } deriving (Show, Generic, Eq)
-- type PkgMap = Map Version (Map SysDesc PackageDesc)


--
--
--
--
-- studioName, defaultInstallFolderName, localPath, atomHome, globalConfigUri, lunaStudioAtomPackageName, runExe :: Text.Text
--
-- -- PACZKOWANIE
-- localPath = "luna-studio-package"
-- globalConfigUri = "https://luna-lang.org/releases/config.yaml"
-- lunaStudioAtomPackageName = "luna-studio" -- name of package in luna.yaml
-- runExe = "run"
-- studioFolder = "atom"
-- --MacOS--
-- binaryLocation = "Contents/MacOS"
-- resources = "Contents/Resources"
-- frameworks = "Contents/Frameworks"
-- appName = "LunaStudio.app"
--
-- --windows--
-- -- binsLocation = "C:\\ProgramFiles"
-- batLocation = "luna\\services"
-- unzipscript = "C:\\Users\\vm\\project\\j_unzip.vbs"
-- pathtounzip = "C:\\Users\\vm\\Downloads\\atom_windows.zip"
-- unzipVBAUri = "https://luna-lang.org/releases/j_unzip.vba"
--
--
-- -- defaultInstallFolderName = ".luna-studio"
-- -- atomHome = "luna-atom" -- -> localName
-- lunaStudioPackage = "luna"
--
-- -- TODO[na koniec]: zrobic lensy




--
-- -- === Utils === --
--
-- scriptDir :: MonadIO m => m FilePath
-- scriptDir = liftIO $ do
--   localExePath <- Environment.getExecutablePath
--   path         <- parseAbsFile localExePath
--   return $ toFilePath (parent path)
--
--
-- -----------------------------
-- -- === Path operations === --
-- -----------------------------
--
-- copyDir ::  MonadIO m => FilePath -> FilePath -> m ()
-- copyDir src dst = liftIO $  do
--   whenM (not <$> doesDirectoryExist src) $
--     throw (userError "source does not exist")
--   whenM (doesFileOrDirectoryExist dst) $
--     throw (userError "destination already exists")
--
--   createDirectoryIfMissing True dst
--   content <- getDirectoryContents src
--   let xs = filter (`notElem` [".", ".."]) content
--   forM_ xs $ \name -> do
--     let srcPath = src </> name
--     let dstPath = dst </> name
--     isDirectory <- doesDirectoryExist srcPath
--     if isDirectory
--       then copyDir srcPath dstPath
--       else copyFile srcPath dstPath
--
--   where
--     doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
--     orM xs = or <$> sequence xs
--     whenM s r = s >>= flip when r
--
--
-- mkPathWithHome :: MonadIO m => [Text.Text] -> m FilePath
-- mkPathWithHome args = liftIO $ do
--     homeDir  <- getHomeDirectory
--     mkRelativePath $ (Text.pack homeDir) : args
--
-- mkRelativePath :: MonadIO m => [Text.Text] -> m FilePath
-- mkRelativePath args = do
--     let strArgs = Text.unpack <$> args
--         folded = foldl (\acc x -> acc </> x) "" strArgs
--         a = normalise $ folded
--     return a
--
--
--   ---------------------------------------------------
--   -- === Luna studio package preparation utils === --
--   ---------------------------------------------------
--
--

--

--
-- unpackTarGzUnix :: MonadIO m => FilePath -> FilePath -> m ()
-- unpackTarGzUnix name path =  runProcess_ $ shell ("cd "++path++ "; tar xf " ++ name ++ " --strip 1")
--
-- unpackZipUnix ::  MonadIO m => FilePath -> FilePath -> m ()
-- unpackZipUnix name path =  runProcess_ $ shell ("cd "++path++ "; unzip " ++ name)
--
-- preparePathRPM :: MonadIO m => Text.Text -> m FilePath
-- preparePathRPM name = mkPathWithHome $ [localPath] ++ (Text.pack <$> ["usr", "share"]) ++ [studioName, name]
--
--
-- copyPackage :: MonadIO m => Package -> FilePath -> m ()
-- copyPackage (Package name version path) dstPath= liftIO $ do
--     case path of
--         Main.URI address -> do
--             downloaded <- downloadFromURL address
--             createDirectoryIfMissing True dstPath
--             if (takeExtension downloaded == ".gz")
--                 then unpackTarGzUnix downloaded dstPath
--                 else if (takeExtension downloaded == ".rpm")
--                     then unpackRPM downloaded dstPath
--                     else if (takeExtension downloaded == ".zip")
--                         then unpackZipUnix downloaded dstPath
--                         else error "Cannot unpack"
--         LocalPath filepath -> do
--             if (takeExtension filepath == ".gz")  then unpackTarGzUnix filepath dstPath else copyDir filepath dstPath
--
--
-- copyBin :: MonadIO m => Package -> FilePath -> m ()
-- copyBin (Package name version path) dstDir = liftIO $ do
--     if name == "luna"
--         then do
--             case path of
--                 LocalPath filepath -> do
--                     sourceFile <- mkRelativePath [(Text.pack filepath), runExe]
--                     dstFile <- mkPathWithHome [(Text.pack dstDir), studioName]
--                     createDirectoryIfMissing True dstDir
--                     copyFile sourceFile dstFile
--                 Main.URI address -> do return ()
--         else return ()
--
-- copyBinRPM :: MonadIO m => Package -> m ()
-- copyBinRPM (Package name version path) = liftIO $ do
--     dstDir <- mkPathWithHome $ [localPath] ++ (Text.pack <$> ["usr", "bin"])
--     copyBin (Package name version path) dstDir
--
-- ---------------------------------
-- -- === Package preparation === --
-- ---------------------------------
--
-- unpackRPM :: MonadIO m => FilePath -> FilePath -> m () -- just for atom rpm unpacking
-- unpackRPM name path = do
--   runProcess_ $ shell ("rpm2cpio " ++ name ++ " | cpio -idmv")
--   runProcess_ $ shell ("mv usr/share/atom/* " ++ path)
--
--
--
--
-- copyPackageForRPM :: MonadIO m => Package -> m ()
-- copyPackageForRPM (Package name version path) = liftIO $ do
--     localPath <- preparePathRPM name
--     copyPackage (Package name version path) localPath
--
--
--
-- mkPackageToRPM :: MonadIO m => FilePath -> m ()
-- mkPackageToRPM yamlFile = liftIO $ do
--     content <- BS.readFile yamlFile
--     let parsedContent = Yaml.decode content :: Maybe LunaVersion
--         LunaVersion lunaID system pkgs = failIfNothing "Could not parse yaml file." parsedContent
--     mapM copyPackageForRPM pkgs
--     home            <- getHomeDirectory
--     lunaStudioRun   <- mkRelativePath [(Text.pack home), localPath ,(Text.pack "usr"),(Text.pack "share"), studioName, lunaStudioPackage, runExe]
--     lunaStudioPosix <- mkRelativePath [(Text.pack home), localPath ,(Text.pack "usr"),(Text.pack "share"), studioName, runPosix]
--
--     copyFile lunaStudioRun lunaStudioPosix
--     -- runProcess_ $ shell ("mv /home/sylwia/luna-studio-package/usr/share/luna-studio/luna/run /home/sylwia/luna-studio-package/usr/share/luna-studio/luna-studio")
--     lunaAtomAbs    <- mkPathWithHome [localPath, "usr", "share", studioName, atomHome]
--     lunaStudioAtom <- mkPathWithHome [lunaAtomAbs, atomHome, "packages", lunaStudioAtomPackageName]
--     createDirectoryIfMissing True lunaStudioAtom
--     studioDir <- mkPathWithHome [localPath, "usr", "share", studioName, lunaStudioPackage,studioFolder]
--     copyDir studioDir lunaAtomAbs
--     localAbsolute <- mkPathWithHome [localPath]
--     -- runProcess_ $ shell ("mv /home/sylwia/luna-studio-package/usr/share/luna-studio/luna/atom/* /home/sylwia/luna-studio-package/usr/share/luna-studio/luna-atom/packages/luna-studio/")
--     runProcess_ $ setWorkingDir lunaStudioAtom (setEnv [("ATOM_HOME", lunaAtomAbs)] $ shell ("apm install ."))
--     runProcess_ $ setWorkingDir home $ shell ("fpm -s dir -t rpm -n 'luna-studio' -v 1.0 -C " ++ localAbsolute ++ " --depends lsb-core-noarch --depends 'libXss.so.1()(64bit)' --depends 'rpmlib(PayloadFilesHavePrefix) <= 4.0-1' --depends 'rpmlib(CompressedFileNames) <= 3.0.4-1' --depends 'libzmq.so.5()(64bit)'")
--
--
--
--
-- lunaStudioDirectory :: MonadIO m => m FilePath
-- lunaStudioDirectory = mkPathWithHome [defaultInstallFolderName]
--
-- ------ ProgressBar -----------------
-- -----------------------------------
--
-- -- === Definition === --
--
-- data ProgressBar = ProgressBar { barWidth  :: Int --total progress bar width
--                                , completed :: Int --Amount of work completed
--                                , totalWork :: Int --total amount of work
--                                }
--
--
-- --------------------------------
-- ------ ProgressBarUtils --------
-- --------------------------------
--
-- progressBar :: MonadIO m => ProgressBar -> m ()
-- progressBar (ProgressBar width todo done) = liftIO $ do
--
--     putStrLn $ printf "[%s%s%s]" (genericReplicate completed '=') (if remaining /= 0 && completed /= 0 then ">" else "") (genericReplicate (remaining - if completed /= 0 then 1 else 0) '.')
--     cursorUpLine 1
--     clearLine
--     where
--         fraction :: Rational
--         fraction | done /= 0  = (fromIntegral todo) % (fromIntegral done)
--                  | otherwise = 0 % 1
--
--         effectiveWidth = max 0 $ width - usedSpace
--         usedSpace = 2
--
--         numCompletedChars :: Rational
--         numCompletedChars = fraction * ((fromIntegral effectiveWidth) % 1)
--
--         completed, remaining :: Int
--         completed = min effectiveWidth $ floor numCompletedChars
--         remaining = effectiveWidth - completed
--
--
--
-- updateProgress :: MonadIO m => ProgressBar -> ConduitM BS.ByteString BS.ByteString m ()
-- updateProgress (ProgressBar w completed pgTotal) = await >>= maybe (return ()) (\chunk -> do
--     let len = BS.length chunk
--         pg = ProgressBar w (completed+len) pgTotal
--     liftIO $ progressBar pg
--     yield chunk
--     updateProgress pg)
--
-- downloadWithProgressBar :: MonadIO m => Text.Text -> m ()
-- downloadWithProgressBar address = liftIO $ withManager $ \manager -> do
--     -- Start the request
--     req <- parseUrl $ Text.unpack address
--     res <- http req manager
--     -- Get the Content-Length and initialize the progress bar
--     let Just cl = lookup hContentLength (responseHeaders res)
--         pgTotal = read (unpack cl)
--         pg      = ProgressBar 50 0 pgTotal
--     -- Consume the response updating the progress bar
--     responseBody res $=+ updateProgress pg $$+- sinkNull
--     liftIO $ putStrLn "Download completed!"
--
--
-- ---------------------------
-- ---InstallerHelpers--------
-- ---------------------------
--
--


--
-- findVersion :: MonadIO m => Text.Text -> Version -> [Version] -> Version -> m Version
-- findVersion question ans prompt defAns
--     | show ans == ""    = return defAns
--     | ans `elem` prompt = return ans
--     | otherwise         = do
--         liftIO $ putStrLn "This version is not available"
--         liftIO $ chooseVersion question prompt defAns
--
-- chooseVersion :: MonadIO m => Text.Text -> [Version] -> Version -> m Version
-- chooseVersion question prompt defAns = do
--     let questionToAsk = Text.concat $ [question] ++ (Text.pack <$> ([" [" ++ (show defAns) ++ "] " ++ "available versions: "] ++ [(show prompt)]))
--     liftIO $ print questionToAsk
--     ans <- liftIO $ getLine
--     findVersion question (read ans) prompt defAns
--
-- getPackagePath :: Text.Text -> Package -> Text.Text
-- getPackagePath nameToCheck (Package name version path) = do
--     case name of
--         nameToCheck -> case path of
--             Main.URI address -> address
--
-- getAddress :: Version -> LunaVersion -> Maybe Text.Text
-- getAddress versionId (LunaVersion lId system pkgs) =
--     if versionId == lId
--         then do
--             let paths = map (getPackagePath studioName) pkgs
--             listToMaybe paths
--         else Nothing
--
-- checkShell :: MonadIO m => m ()
-- checkShell = liftIO $ do
--     (exitCode, out, err) <- readProcess (shell "help")
--     exportPaths $ getShellType $ show out
--
-- check :: Eq a => [a] -> [a] -> Bool
-- check l s = check' l s True where
--     check' _ [] h          = True
--     check' [] _ h          = False
--     check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)
--
-- getShellType :: String -> String
-- getShellType help = head $ splitOn "," help
--
--
-- exportPaths :: MonadIO m => String -> m ()
-- exportPaths shellType
--     | check shellType "bash" = do
--         bashrcLocation      <- mkPathWithHome [".bashrc"]
--         bashrcCheck         <- liftIO $ doesPathExist bashrcLocation
--         bashProfileLocation <- mkPathWithHome [".bash_profile"]
--         bashProfileCheck    <- liftIO $ doesPathExist bashProfileLocation
--         profileLocation     <- mkPathWithHome [".profile"]
--         profileCheck        <- liftIO $ doesPathExist profileLocation
--         let export = "export PATH=~/.local/bin:$PATH"
--         if  bashrcCheck
--             then liftIO $ appendFile bashrcLocation export
--             else if bashProfileCheck
--                 then liftIO $ appendFile bashProfileLocation export
--                 else if profileCheck
--                     then liftIO $ appendFile profileLocation export
--                     else liftIO $ putStrLn "Unrecognized shell. Please add ~/.local/bin to your exports."
--     | check shellType "zsh" = liftIO $ do
--         zshrcLocation <- mkPathWithHome [".zshrc"]
--         let export = "path+=~/.local/bin"
--         liftIO $ appendFile zshrcLocation export
--     | check shellType "fish" = liftIO $ do
--         fishrcLocation <- mkPathWithHome [".config", "fish", "config.fish"]
--         let export = "set -gx PATH $PATH ~/.local/bin"
--         liftIO $ appendFile fishrcLocation export
--     | otherwise = liftIO $ putStrLn "Unrecognized shell. Please add ~/.local/bin to your exports."
--
--
-- makeExecutable :: MonadIO m => FilePath -> m ()
-- makeExecutable file = liftIO $ do
--     p <- getPermissions file
--     setPermissions file (p {writable = True})
--

--
--
--
-- ---------------------------------------
-- -------------MacOS---------------------
-- ---------------------------------------
--
-- mkPackageMacOS :: MonadIO m => FilePath -> m()
-- mkPackageMacOS yamlFile = liftIO $ do
--     content <- BS.readFile yamlFile
--     let parsedContent = Yaml.decode content :: Maybe LunaVersion
--         LunaVersion lunaID system pkgs = failIfNothing "Could not parse yaml file." parsedContent
--         version        <- show(lunaID)
--         binPath        <- mkPathWithHome [appName, binaryLocation]
--         resourcesPath  <- mkPathWithHome [appName, resources, version]
--         frameworksPath <- mkPathWithHome [appName, frameworks, version]
--         createDirectory binPath
--         createDirectory resourcesPath
--         createDirectory frameworksPath
--         for packages if package atom or zmq copyPackage LunaStudio.app/Contents/Frameworks/v else createDirectory LunaStudio.app/Contents/Resources/v
--
--         copyBin LunaStudio.app/Contents/MacOS/LunaStudio -- sprobowac run odpala resources/current zawsze a lunaatom path z uwzglednieniem wersji jest na podstawie pliku w resources z wersja
--
--         link each to current (frameworks and resources)
--         podmienic jeszcze ikonke z atoma na lune i nazwe?
--
-- ---------------------------------------
-- -------------Windows-------------------
-- ---------------------------------------
--
--
-- mkPathWithHomeWindows :: MonadIO m => [Text.Text] -> m FilePath
-- mkPathWithHomeWindows args = liftIO $ do
--     homeDir  <- getHomeDirectory
--     mkRelativePathWindows $ (Text.pack homeDir) : args
--
-- mkRelativePathWindows :: MonadIO m => [Text.Text] -> m FilePath
-- mkRelativePathWindows args = do
--     let strArgs = Text.unpack <$> args
--         folded = foldl (\acc x -> acc Win.</> x) "" strArgs
--         a = normalise $ folded
--     return a
--
-- -- mkPackageWin :: MonadIO m => m ()
-- -- mkPackageWin = do
-- --     content <- BS.readFile yamlFile
-- --     let parsedContent = Yaml.decode content :: Maybe LunaVersion
-- --         LunaVersion lunaID system pkgs = failIfNothing "Could not parse yaml file." parsedContent
--         --
--
--
-- installWin :: MonadIO m => m ()
-- installWin = liftIO $ do
--     tmp <- getTemporaryDirectory
--     setCurrentDirectory tmp
--     downoladConfig <- downloadFromURL globalConfigUri
--     downloadUnzipScript <- downloadFromURL unzipVBAUri
--
--     content <- BS.readFile downoladConfig
--     let parsedContent = Yaml.decode content :: Maybe [LunaVersion]
--         lunaVersions = failIfNothing "Could not parse yaml file." parsedContent
--         allLunaIds = map Main.id lunaVersions
--         lastLunaId = maximum allLunaIds
--     absDefault       <- mkPathWithHomeWindows [defaultInstallFolderName]
--     let location         = binsLocation
--     versionToinstall <- chooseVersion (Text.pack "Which version of Luna-Studio you want to install?")   allLunaIds lastLunaId
--
--     let address = mapM (getAddress versionToinstall) lunaVersions
--         justAddressesList = failIfNothing "cannot read URI" address
--         justAddress = failIfNothing "cannot read URI" $ listToMaybe justAddressesList
--     locWithVersion <- mkRelativePathWindows [location, (Text.pack $ show versionToinstall)]
--     createDirectory locWithVersion
--     setCurrentDirectory locWithVersion
--     runProcess_ $ shell ("cscript //B " ++ downloadUnzipScript ++ " " ++ (Text.unpack justAddress))
--     batAbs <- mkRelativePathWindows [(Text.pack locWithVersion), batLocation]
--     runProcess_ $ setWorkingDir batAbs $ shell "installAll"
--
--
--
--
--
--
-- data Opts = Opts
--     { optGlobalFlag :: !Bool
--     , optCommand    :: !Command
--     }
--
-- data Command = Install | MakeRPM String | Update
--
--
-- main :: IO ()
-- main = do
--     opts <- execParser optsParser
--     case optCommand opts of
--       Install  -> installAppImage
--       MakeRPM config -> mkPackageToRPM config
--       Update -> putStrLn "powinno sciagac nowy config z luna-lang org albo czegos takiego"
--   where
--       optsParser =
--           info
--               (helper <*> versionOption <*> programOptions)
--               (fullDesc <> progDesc "LunaStudio manager" <>
--                header
--                    "manager for LunaStudio for makeing and instaling packages")
--       versionOption = infoOption "0.0" (long "version" <> help "Show version")
--       programOptions =
--         Opts <$> switch (long "global-flag" <> help "Set a global flag") <*>
--         hsubparser (installCommand <> mkpkgCommand <> updateCommand)
--       installCommand =
--         command
--           "install"
--           (info (pure Install) (progDesc "Install Luna-Studio"))
--       mkpkgCommand =
--         command
--           "make-rpm"
--           (info mkpkgOptions (progDesc "Make RPM"))
--       mkpkgOptions =
--         MakeRPM <$>
--         strArgument (metavar "CONFIG" <> help "Name of config file")
--       updateCommand =
--         command
--           "update"
--           (info (pure Update) (progDesc "update luna"))
