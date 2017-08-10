{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where

import Control.Monad.Raise

import Luna.Manager.Shell.Commands
import Luna.Manager.Network
import Luna.Manager.System.Host

import Prologue hiding (FilePath)

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, filename, directory, extension, basename, parent, dirname)
import qualified Shelly.Lifted as Shelly
import           Shelly.Lifted (MonadSh, (-|-))
import qualified System.Process.Typed as Process
import qualified Data.Text as Text
default (Text.Text)

data ExtensionError = ExtensionError deriving (Show)
instance Exception ExtensionError

extensionError :: SomeException
extensionError = toException ExtensionError

unpack :: (MonadIO m, MonadNetwork m) => FilePath -> m FilePath
unpack file = putStrLn "Unpacking archive" >> case currentHost of
    Windows ->  do
        ext <- tryJust extensionError $ extension file
        case ext of
            "zip" -> unzipFileWindows file
            "gz"  -> untarWin file
    Darwin  -> do
        ext <- tryJust extensionError $ extension file
        case ext of
            "gz"  -> Shelly.shelly $ unpackTarGzUnix file
            "zip" -> Shelly.shelly $ unzipUnix file
    Linux   -> do
        ext <- tryJust extensionError $ extension file
        case ext of
            "AppImage" -> return file
            "gz"       -> Shelly.shelly $ unpackTarGzUnix file
            "rpm"      -> Shelly.shelly $ do
                let name = basename file
                    dir = directory file
                    fullFilename = filename file
                Shelly.mkdir_p $ dir </> name
                Shelly.cp_r file $ dir </> name
                unpackRPM (dir </> name </> fullFilename) (dir </> name)
                return $ dir </> name

unzipUnix :: Shelly.MonadSh m => FilePath -> m FilePath
unzipUnix file = do
    let dir = directory file
        name = basename file
    Shelly.cd dir
    Shelly.mkdir_p name
    Shelly.cp file name
    Shelly.cd $ dir </> name
    out <- Shelly.cmd  "unzip" $ dir </> name </> (filename file)
    Shelly.rm $ dir </> name </> (filename file)
    listed <- Shelly.ls $ dir </> name
    if length listed == 1 then return $ head listed else return $ dir </> name

unpackTarGzUnix :: Shelly.MonadSh m => FilePath -> m FilePath
unpackTarGzUnix file = do
    let dir = directory file
        name = basename file
    Shelly.cd dir
    Shelly.mkdir_p name
    Shelly.cmd  "tar" "-xpzf" file "--total" "--strip=1" "-C" name
    return $ dir </> name

-- TODO: download unzipper if missing
unzipFileWindows :: (MonadIO m, MonadNetwork m)=> FilePath -> m FilePath
unzipFileWindows zipFile = do
    let scriptPath = "https://s3-us-west-2.amazonaws.com/packages-luna/windows/j_unzip.vbs"
    --sprawdź czy jest na dysku, shelly.find, skrypt i plik musza byc w tym samym directory

    script <- downloadFromURL "Downloading archiving tool" scriptPath
    let dir = directory zipFile
        name = dir </> basename zipFile
    -- Shelly.shelly $ Shelly.cp script dir
    Shelly.shelly $ do
      Shelly.cd dir
      Shelly.mkdir_p name
      Shelly.cp zipFile name
      Shelly.cp script name
      Shelly.cd $ dir </> name
      Shelly.cmd "cscript" (filename script) (filename zipFile)
      Shelly.rm $ dir </> name </> (filename zipFile)
      Shelly.rm $ dir </> name </> (filename script)
      listed <- Shelly.ls $ dir </> name
      if length listed == 1
          then do
              liftIO $ print $ Shelly.toTextIgnore $ head listed
              return $ head listed
              else do
                  liftIO $ print $ Shelly.toTextIgnore $ dir </> name
                  return $ dir </> name

untarWin :: (MonadIO m, MonadNetwork m)=> FilePath -> m FilePath
untarWin zipFile = do
  let scriptPath = "https://s3-us-west-2.amazonaws.com/packages-luna/windows/tar.exe"
  --sprawdź czy jest na dysku, shelly.find, skrypt i plik musza byc w tym samym directory

  script <- downloadFromURL scriptPath "Downloading archiving tool"
  let dir = directory zipFile
      name = dir </> basename zipFile
  -- Shelly.shelly $ Shelly.cp script dir
  Shelly.shelly $ Shelly.silently $ do
    Shelly.cd dir
    Shelly.mkdir_p name
    -- Shelly.cp zipFile name
    Shelly.cp script dir
    -- Shelly.cd $ dir </> name
    liftIO $ print name
    Shelly.cmd (dir </> filename script) "untar" (filename zipFile) name
    -- Shelly.rm $ dir </> name </> (filename zipFile)
    -- Shelly.rm $ dir </> name </> (filename script)
    listed <- Shelly.ls $ dir </> name
    liftIO $ print listed
    if length listed == 1
        then do
            liftIO $ print $ Shelly.toTextIgnore $ head listed
            return $ head listed
            else do
                liftIO $ print $ Shelly.toTextIgnore $ dir </> name
                return $ dir </> name

zipFileWindows :: (MonadIO m, MonadNetwork m)=> FilePath -> Text -> m FilePath
zipFileWindows folder appName = do
    let name = parent folder </> Shelly.fromText (appName <> ".tar.gz")
    let scriptPath = "https://s3-us-west-2.amazonaws.com/packages-luna/windows/tar.exe"
    script <- downloadFromURL scriptPath "Downloading archiving tool"
    Shelly.shelly $ do
        Shelly.cd $ parent folder
        Shelly.cp script $ parent folder
        Shelly.cmd (parent folder </> filename script) "tar" name folder
        return name

unpackRPM :: MonadIO m => FilePath -> FilePath -> m ()
unpackRPM file filepath = liftIO $ Process.runProcess_ $ Process.setWorkingDir (encodeString filepath) $ Process.shell $ "rpm2cpio " ++ (encodeString file) ++ " | cpio -idmv"

createTarGzUnix :: Shelly.MonadSh m => FilePath  -> Text -> m FilePath
createTarGzUnix folder appName = do
    let name =  parent folder </> Shelly.fromText (appName <> ".tar.gz")
    Shelly.cd $ parent folder
    Shelly.cmd "tar" "-cpzf" name $ filename folder
    return name
