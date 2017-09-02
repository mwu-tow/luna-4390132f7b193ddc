{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where



import           Luna.Manager.Shell.Shelly (MonadSh)
import           Control.Monad.Raise
import           Data.Aeson (encode)
import           Data.IORef
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, filename, directory, extension, basename, parent, dirname)
import           Luna.Manager.Gui.InstallationProgress
import           Luna.Manager.Network
import           Luna.Manager.Shell.Commands
import           Luna.Manager.System.Host
import           Prologue hiding (FilePath)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified System.Process.Typed as Process
import           System.Exit
import           System.IO (hFlush, stdout, hGetContents)
default (Text.Text)

data ExtensionError = ExtensionError deriving (Show)
instance Exception ExtensionError

extensionError :: SomeException
extensionError = toException ExtensionError

unpack :: (MonadIO m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m) => Bool -> Double -> Text.Text -> FilePath -> m FilePath
unpack guiInstaller totalProgress progressFieldName file = do
    if guiInstaller then return () else putStrLn "Unpacking archive"
    case currentHost of
        Windows ->  do
            ext <- tryJust extensionError $ extension file
            case ext of
                "zip" -> unzipFileWindows file
                "gz"  -> untarWin file
        Darwin  -> do
            ext <- tryJust extensionError $ extension file
            case ext of
                "gz"  -> unpackTarGzUnix guiInstaller totalProgress progressFieldName file
                "zip" -> unzipUnix file
        Linux   -> do
            ext <- tryJust extensionError $ extension file
            case ext of
                "AppImage" -> return file
                "gz"       -> unpackTarGzUnix guiInstaller totalProgress progressFieldName file
                "rpm"      -> do
                    let name = basename file
                        dir = directory file
                        fullFilename = filename file
                    Shelly.mkdir_p $ dir </> name
                    Shelly.cp_r file $ dir </> name
                    unpackRPM (dir </> name </> fullFilename) (dir </> name)
                    Shelly.rm $ dir </> name </> (filename file)
                    return $ dir </> name

unzipUnix :: (MonadSh m, Shelly.MonadShControl m) => FilePath -> m FilePath
unzipUnix file = do
    let dir = directory file
        name = basename file
    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        Shelly.cp file name
        Shelly.chdir (dir </> name) $ do
            out <- Shelly.silently $ Shelly.cmd  "unzip" $ dir </> name </> filename file
            Shelly.rm $ dir </> name </> filename file
            listed <- Shelly.ls $ dir </> name
            if length listed == 1 then return $ head listed else return $ dir </> name

logger :: Text.Text -> Double -> IORef Int -> Int -> Text.Text -> IO ()
logger progressFieldName totalProgress lastNumber n t = do
    modifyIORef' lastNumber succ
    currentFileNumber <- readIORef lastNumber
    let progress = (fromIntegral  currentFileNumber / fromIntegral n :: Double) * totalProgress
    print $ "{\"" <> (convert progressFieldName) <> "\":\"" <> (show progress) <> "\"}"

unpackTarGzUnix :: (MonadSh m, Shelly.MonadShControl m, MonadIO m) => Bool -> Double -> Text.Text -> FilePath -> m FilePath
unpackTarGzUnix guiInstaller totalProgress progressFieldName file = do
    let dir = directory file
        name = basename file
    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        if guiInstaller then do
            (stdout, stderr, sysExit) <- Process.readProcess $ Process.shell "tar -tzf taruj.tar.gz | wc -l"
            let n = Text.decimal $ Text.strip $ Text.decodeUtf8 $ BSL.toStrict stderr
            case n of
                Right x -> do
                    currentUnpackingFileNumber <- liftIO $ newIORef 0
                    Shelly.log_stderr_with (logger progressFieldName totalProgress currentUnpackingFileNumber $ fst x) $ Shelly.cmd "tar" "-xvpzf" (Shelly.toTextIgnore file) "--strip=1" "-C" (Shelly.toTextIgnore name)-- (\stdout -> liftIO $ hGetContents stdout >> print "33")
                Left err -> error err --TODO zamień error na exception
            else void $ Shelly.cmd  "tar" "-xpzf" file "--strip=1" "-C" name
        return $ dir </> name

-- TODO: download unzipper if missing
unzipFileWindows :: (MonadIO m, MonadNetwork m)=> FilePath -> m FilePath
unzipFileWindows zipFile = do
    let scriptPath = "http://packages.luna-lang.org/windows/j_unzip.vbs"
    --sprawdź czy jest na dysku, shelly.find, skrypt i plik musza byc w tym samym directory
    script <- downloadFromURL scriptPath "Downloading archiving tool"
    let dir = directory zipFile
        name = dir </> basename zipFile
    -- Shelly.shelly $ Shelly.cp script dir
    Shelly.shelly $ do
      Shelly.chdir dir $ do
          Shelly.mkdir_p name
          Shelly.cp zipFile name
          Shelly.cp script name
      Shelly.chdir (dir </> name) $ do
          Shelly.cmd "cscript" (filename script) (filename zipFile)
          Shelly.rm $ dir </> name </> filename zipFile
          Shelly.rm $ dir </> name </> filename script
          listed <- Shelly.ls $ dir </> name
          if length listed == 1
              then do
                  liftIO $ print $ Shelly.toTextIgnore $ head listed
                  return $ head listed
                  else do
                      liftIO $ print $ Shelly.toTextIgnore $ dir </> name
                      return $ dir </> name

untarWin :: (MonadIO m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m)=> FilePath -> m FilePath
untarWin zipFile = do
  let scriptPath = "http://packages.luna-lang.org/windows/tar.exe"
  --sprawdź czy jest na dysku, shelly.find, skrypt i plik musza byc w tym samym directory

  script <- downloadFromURL scriptPath "Downloading archiving tool"
  let dir = directory zipFile
      name = dir </> basename zipFile
  Shelly.silently $ do
    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        -- Shelly.cp script dir
        -- liftIO $ print name
        Shelly.cmd (dir </> filename script) "untar" (filename zipFile) name
        listed <- Shelly.ls $ dir </> name
        if length listed == 1
            then do
                return $ head listed
                else do
                    return $ dir </> name

zipFileWindows :: (MonadIO m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m)=> FilePath -> Text -> m FilePath
zipFileWindows folder appName = do
    let name = parent folder </> Shelly.fromText (appName <> ".tar.gz")
    let scriptPath = "http://packages.luna-lang.org/windows/tar.exe"
    script <- downloadFromURL scriptPath "Downloading archiving tool"
    Shelly.chdir (parent folder) $ do
        Shelly.cp script $ parent folder
        Shelly.cmd (parent folder </> filename script) "tar" name folder
        return name

unpackRPM :: MonadIO m => FilePath -> FilePath -> m ()
unpackRPM file filepath = liftIO $ do
    (exitCode, out, err) <- Process.readProcess $ Process.setWorkingDir (encodeString filepath) $ Process.shell $ "rpm2cpio " <> encodeString file <> " | cpio -idmv"
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure a -> print $ "Fatal: rpm not unpacked. " <> err

createTarGzUnix :: (MonadSh m, Shelly.MonadShControl m) => FilePath  -> Text -> m FilePath
createTarGzUnix folder appName = do
    let name =  parent folder </> Shelly.fromText (appName <> ".tar.gz")
    Shelly.chdir (parent folder) $ do
        Shelly.cmd "tar" "-cpzf" name $ filename folder
        return name
