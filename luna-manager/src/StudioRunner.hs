
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

module Main where

import Prelude
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy  (unpack)
import Data.List.Split
import System.Directory (doesDirectoryExist, setCurrentDirectory, getHomeDirectory, getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.FilePath ((</>), normalise)
import System.Process.Typed (shell, runProcess, runProcess_, setWorkingDir, setEnv, readProcess_)
import System.Environment (getExecutablePath)
import qualified System.Environment  as Environment
import System.IO
import qualified  System.FilePath.Windows as Win
import Data.Maybe (fromMaybe)
import Path (parent, parseAbsFile, toFilePath)

import           Luna.Manager.System.Host

import qualified Data.Text as T
default (T.Text)

version = "1.0.0"
atomHome = "local"
atomFolderName = "atom"
studioHome = "luna-atom"
logs = "logs"
atomPackageName = "luna-studio"
lunaFolderName = "luna"
supervisorFolderName = "supervisor"
supervisordFolderName = "supervisord"
supervisordBin = "supervisord"
current = "current" --just for MacOS
atomAppBin = "Atom.app/Contents/MacOS"
atomFolderWin = "Atom"
atomBinWin = "atom"

scriptDir :: IO FilePath
scriptDir = liftIO $ do
  localExePath <- getExecutablePath
  path         <- parseAbsFile localExePath
  return $ toFilePath (parent path)

-----Linux----

lunaAtomHome :: IO FilePath
lunaAtomHome = do
  home <- getHomeDirectory
  prepPathNoHome [home, ".luna-studio", atomHome]

logsDir :: IO FilePath
logsDir = do
  home <- getHomeDirectory
  prepPath [home, ".luna-studio", logs]

backendDir :: IO FilePath
backendDir = prepPath [ lunaFolderName, supervisorFolderName]

pathLunaAtomHomePackage ::IO FilePath
pathLunaAtomHomePackage = liftIO $ do
    home         <- getHomeDirectory
    lunaAtomHome <- prepPathNoHome [home, studioHome, version, atomHome, "packages", atomPackageName]
    return lunaAtomHome

----MacOS----

mainDirMacOS :: IO FilePath
mainDirMacOS = liftIO $ do
    localExePath <- getExecutablePath
    path     <- parseAbsFile localExePath
    return $ toFilePath $ parent path

resourcesMacOS :: IO FilePath
resourcesMacOS = liftIO $ do
    contents <- mainDirMacOS
    resources <- prepPathNoHome [contents, "Resources" ]
    return resources

frameworksMacOS :: IO FilePath
frameworksMacOS = liftIO $ do
    contents <- mainDirMacOS
    frameworks <- prepPathNoHome [contents, "Frameworks" ]
    return frameworks

backendDirMacOS :: IO FilePath
backendDirMacOS = liftIO $ do
    resources <- mainDirMacOS
    backend <- prepPathNoHome [resources, lunaFolderName, supervisorFolderName]
    return backend

supervisordMacOS :: IO FilePath
supervisordMacOS = liftIO $ do
    resources   <- mainDirMacOS
    supervisord <- prepPathNoHome [resources, supervisordFolderName, supervisordBin]
    return supervisord

atomMacOS :: IO FilePath
atomMacOS = liftIO $ do
    frameworks <- frameworksMacOS
    atom       <- prepPathNoHome [frameworks, atomFolderName, current, atomAppBin]
    return atom

------Windows------

prepPath :: [String] -> IO FilePath
prepPath args = do
  localDir <- scriptDir
  let fromHome = foldl (\acc x -> acc </> x) "" args
  return $ normalise $ localDir </> fromHome

prepPathNoHome :: [String] -> IO FilePath
prepPathNoHome args = do
  let fromHome = foldl (\acc x -> acc </> x) "" args
  return $ normalise $ fromHome

--Linux

copyLunaStudio :: IO ()
copyLunaStudio = do
  mainDir <- scriptDir
  atomHome <- lunaAtomHome
  createDirectoryIfMissing True atomHome
  runProcess_ $ shell ("cp -R " ++ mainDir ++ studioHome ++ "* " ++ atomHome)

checkLunaHome :: IO ()
checkLunaHome = do
  home            <- getHomeDirectory
  pathLunaPackage <- pathLunaAtomHomePackage
  doesDirectoryExist pathLunaPackage >>= \case
    True -> return ()
    False -> copyLunaStudio


runLunaEmpire :: IO ()
runLunaEmpire = do
  a <- backendDir
  c <- scriptDir
  logs <- logsDir
  createDirectoryIfMissing True logs

  runProcess_ $ shell ("cd "++ a ++ "; " ++ c ++ "supervisord/supervisord -c supervisord-package.conf")

runLinux :: IO ()
runLinux = do
    currentDirr <- scriptDir
    Environment.setEnv "LUNAATOM" currentDirr
    checkLunaHome
    runLunaEmpire

--MacOS
copyLunaStudioMacOS :: IO ()
copyLunaStudioMacOS = do
  resources <- mainDirMacOS

  atomHome <- lunaAtomHome

  createDirectoryIfMissing True atomHome
  studioHomePath <- prepPathNoHome [resources, studioHome, "*"]
  runProcess_ $ shell ("cp -R " ++ studioHomePath ++ " " ++ atomHome)

checkLunaHomeMacOS :: IO ()
checkLunaHomeMacOS = do
  home <- getHomeDirectory
  pathLunaPackage <- pathLunaAtomHomePackage
  doesDirectoryExist pathLunaPackage >>= \case
    True -> return ()
    False -> copyLunaStudioMacOS


runLunaEmpireMacOS :: IO ()
runLunaEmpireMacOS = do
  lunaSupervisor <- backendDirMacOS
  supervisord    <- supervisordMacOS
  logs <- logsDir
  print lunaSupervisor
  print supervisord
  createDirectoryIfMissing True logs
  runProcess_ $ setWorkingDir lunaSupervisor $ shell (supervisord  ++" -c supervisord-mac.conf")


runMacOS :: IO ()
runMacOS = do
    atom           <- atomMacOS
    Environment.setEnv "LUNAATOM" atom
    checkLunaHomeMacOS
    runLunaEmpireMacOS

--Windows

mkPathWithHomeWindows :: MonadIO m => [String] -> m FilePath
mkPathWithHomeWindows args = liftIO $ do
    homeDir  <- getHomeDirectory
    mkRelativePathWindows $ homeDir : args

mkRelativePathWindows :: MonadIO m => [String] -> m FilePath
mkRelativePathWindows args = do
    let folded = foldl (\acc x -> acc Win.</> x) "" args
        a = normalise $ folded
    return a

logsWin :: IO FilePath
logsWin = mkPathWithHomeWindows [studioHome, version, logs]

atomHomePathWin :: IO FilePath
atomHomePathWin = mkPathWithHomeWindows [studioHome, version, atomHome]

pathLunaAtomHomePackageWin :: IO FilePath
pathLunaAtomHomePackageWin =  do
    atomHomePath <- atomHomePathWin
    path <- mkRelativePathWindows [atomHomePath, "packages", atomPackageName]
    return path

atomPathWin :: IO FilePath
atomPathWin = liftIO $ do
    script   <- scriptDir
    atomFold <- mkRelativePathWindows [script, atomFolderWin]
    return atomFold

checkLunaHomeWin :: IO ()
checkLunaHomeWin = liftIO $ do
    pathLunaPackage <- pathLunaAtomHomePackageWin
    doesDirectoryExist pathLunaPackage >>= \case
      True -> return ()
      False -> copyLunaStudioWin

copyLunaStudioWin :: IO ()
copyLunaStudioWin = liftIO $ do
    mainDir <- scriptDir
    atomHome <- atomHomePathWin
    createDirectoryIfMissing True atomHome
    runProcess_ $ shell ("xcopy " ++ mainDir ++ studioHome ++ " " ++ atomHome ++ " /e /i /h")


runWindows :: IO ()
runWindows = do
    currentDirr <- scriptDir
    logs <- logsWin
    atom <- atomPathWin
    atomHomeAbs <- atomHomePathWin
    createDirectoryIfMissing True logs
    checkLunaHomeWin
    runProcess_ $ setWorkingDir atom $ setEnv [("ATOM_HOME", atomHomeAbs)] $ shell ("atom")


main :: IO ()
main = case currentHost of
    Linux -> do
        currentDirr <- scriptDir
        Environment.setEnv "LUNAATOM" currentDirr
        checkLunaHome
        runLunaEmpire
    Darwin -> runMacOS
