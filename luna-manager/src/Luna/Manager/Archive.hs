{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where

import Control.Monad.Raise

import Luna.Manager.Shell.Commands
import Luna.Manager.Network
import Luna.Manager.System.Host

import Luna.Manager.Gui.InstallationProgress
import System.IO (hFlush, stdout)
import Data.Aeson (encode)

import Prologue hiding (FilePath)

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, filename, directory, extension, basename, parent, dirname)
import qualified Luna.Manager.Shell.Shelly as Shelly
import           Luna.Manager.Shell.Shelly (MonadSh)
import System.Exit
import qualified System.Process.Typed as Process
import qualified Data.Text as Text
default (Text.Text)

data ExtensionError = ExtensionError deriving (Show)
instance Exception ExtensionError

extensionError :: SomeException
extensionError = toException ExtensionError

unpack :: (MonadIO m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m) => Bool -> FilePath -> m FilePath
unpack guiInstaller file = do
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
                "gz"  -> unpackTarGzUnix guiInstaller file
                "zip" -> unzipUnix file
        Linux   -> do
            ext <- tryJust extensionError $ extension file
            case ext of
                "AppImage" -> return file
                "gz"       -> unpackTarGzUnix guiInstaller file
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

unpackTarGzUnix :: (MonadSh m, Shelly.MonadShControl m) => Bool -> FilePath -> m FilePath
unpackTarGzUnix guiInstaller file = do
    let dir = directory file
        name = basename file
    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        if guiInstaller then Shelly.cmd  "tar" "-xpzfv" file "--strip=1" "-C" name

            else Shelly.cmd  "tar" "-xpzf" file "--strip=1" "-C" name
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
