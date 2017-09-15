#!/usr/bin/env stack
-- stack --resolver lts-8.2 --install-ghc runghc --package base --package exceptions --package shelly --package text --package directory --package system-filepath --verbose -- -hide-all-packages
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
import qualified Shelly.Lifted as Shelly
import Shelly.Lifted (MonadSh, (</>), shelly, liftIO)
import qualified System.Directory as System
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad (when)
import Control.Monad.IO.Class ( MonadIO)
import Control.Exception (Exception)
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.IO (writeFile)
import Filesystem.Path.CurrentOS (parent, encodeString)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
default (T.Text)

stack = "../../tools/stack/stack"

tools = "../../tools"
libs = "../../libs"
supportedNodeVersion = "6.11.3"
supportedPythonVersion = "3.6.2"
lunaShell = "./luna-shell.sh"

currentPath :: (MonadSh m, MonadIO m) => m Text
currentPath = do
    path <- Shelly.get_env "APP_PATH"
    currentDirectory <- liftIO $ System.getCurrentDirectory
    return $ fromMaybe (T.pack currentDirectory) path

-------------------
-- === Hosts === --
-------------------

-- === Definition === --

data System = Linux
            | Darwin
            | Windows
            deriving (Show)


-- === System discovery === --

currentHost :: System


#ifdef linux_HOST_OS
currentHost      =  Linux
#elif darwin_HOST_OS
currentHost      =  Darwin
#elif mingw32_HOST_OS
currentHost      =  Windows
#else
Running on unsupported system.
#endif

sanityCheck :: (MonadSh m, Shelly.MonadShControl m) => Shelly.FilePath -> [T.Text] -> m ()
sanityCheck command params = Shelly.silently $ do
    Shelly.errExit False $ bashLogin command params
    exit <- Shelly.lastExitCode
    when (exit /= 0) $ Shelly.errorExit (Shelly.toTextIgnore command)

pythonLibs :: [T.Text]
pythonLibs = ["requests"]

installPython :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installPython = do
    Shelly.echo "installing python locally"
    current <- currentPath
    let pythonFolder = current </> tools </> "python"
    Shelly.chdir_p pythonFolder $ do
        Shelly.cmd "git" ["clone", "https://github.com/pyenv/pyenv.git"]
        Shelly.setenv "PYENV_ROOT" $ Shelly.toTextIgnore $ pythonFolder </> "pyenv"
        Shelly.prependToPath $ pythonFolder </> "pyenv" </> "bin"
        Shelly.prependToPath $ pythonFolder </> "pyenv" </> "shims"
        Shelly.cmd "pyenv" ["init", "-"]
        Shelly.cmd "pyenv" ["install", supportedPythonVersion]
        Shelly.cmd "pyenv" ["local", supportedPythonVersion]
        Shelly.cmd "pip" $ "install" : pythonLibs

installNode :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installNode = do
    Shelly.echo "installing node locally"
    current <- currentPath
    let nodeFolder = current </> tools </> "node"
    Shelly.chdir_p nodeFolder $ do
        Shelly.mkdir_p supportedNodeVersion
        case currentHost of
            Linux  -> do
                Shelly.cmd "wget" ["https://nodejs.org/dist/v6.11.3/node-v6.11.3-linux-x64.tar.xz"]
                Shelly.cmd  "tar" "-xpJf" "./node-v6.11.3-linux-x64.tar.xz" "--strip=1" "-C" supportedNodeVersion
                Shelly.rm "./node-v6.11.3-linux-x64.tar.xz"
            Darwin -> do
                Shelly.cmd "wget" ["https://nodejs.org/dist/v6.11.3/node-v6.11.3-darwin-x64.tar.gz"]
                Shelly.cmd  "tar" "-xpzf" "./node-v6.11.3-darwin-x64.tar.gz" "--strip=1" "-C" supportedNodeVersion
                Shelly.rm "./node-v6.11.3-darwin-x64.tar.gz"

nodeModules :: [T.Text]
nodeModules = ["less"]

installNodeModules :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installNodeModules = do
    Shelly.echo "installing node modules"
    current <- currentPath
    let nodeBinPath = current </> tools </> "node" </> supportedNodeVersion </> "bin"
    Shelly.prependToPath nodeBinPath
    Shelly.cmd (nodeBinPath </> "npm") $ "install" : nodeModules


haskellBins :: [T.Text]
haskellBins = [
      "hprotoc"
    , "happy"
    , "hsc2hs"
    ]

installHaskellBins :: (MonadSh m, Shelly.MonadShControl m) => m ()
installHaskellBins = do
    Shelly.cmd stack $ "install" : haskellBins
    sanityCheck "happy" ["--version"]

downloadLibs :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
downloadLibs = do
    Shelly.echo "downloading libraries"
    current <- currentPath
    let libsFolder = current </> libs
    Shelly.chdir_p (parent libsFolder) $ do
        case currentHost of
            Linux  -> do
                Shelly.cmd "wget" ["https://s3-us-west-2.amazonaws.com/packages-luna/linux/studio/libs/luna-studio-libs.tar.gz"]
                Shelly.cmd  "tar" "-xpzf" "./luna-studio-libs.tar.gz" "--strip=1"
                Shelly.rm "./luna-studio-libs.tar.gz"
            Darwin -> do
                Shelly.cmd "wget" ["https://s3-us-west-2.amazonaws.com/packages-luna/darwin/studio/libs/luna-studio-libs.tar.gz"]
                Shelly.cmd  "tar" "-xpzf" "./luna-studio-libs.tar.gz" "--strip=1"
                Shelly.rm "./luna-studio-libs.tar.gz"

bashLogin :: MonadSh m => Shelly.FilePath -> [T.Text] -> m T.Text
bashLogin command params = do
    Shelly.cmd "bash" ["-c", "-l", (Shelly.toTextIgnore command) `T.append` " " `T.append` T.intercalate " " params]

intercalatePaths :: [Shelly.FilePath] -> Text
intercalatePaths filepaths = intercalate ":" $ Shelly.toTextIgnore <$> filepaths

generateLunaShellScript :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
generateLunaShellScript = do
    Shelly.echo "generate luna shell"
    current <- currentPath
    let lbsPath            = current </> libs
        pyenvShimsFolder   = current </> tools </> "python" </> "pyenv" </> "shims"
        pyenvBinFolder     = current </> tools </> "python" </> "pyenv" </> "bin"
        stackPath          = current </> stack
        nodeBinPath        = current </> tools </> "node" </> supportedNodeVersion </> "bin"
        addLdLibraryPath   = "export LD_LIBRARY_PATH=" ++ encodeString lbsPath
        paths              = T.unpack $ intercalatePaths [stackPath, pyenvShimsFolder, pyenvBinFolder, nodeBinPath]
        addPath            = "export PATH="++ paths ++ ":$PATH"
        pyenvEnviromentVar = "export PYENV_ROOT=" ++ (encodeString $ current </> tools </> "python" </> "pyenv")
        loadPython         = "pyenv" ++  " local " ++ encodeString supportedPythonVersion
        shellCmd           = "bash"
        lunaShellPath      = current </> lunaShell
        fullCode           = T.unlines $ T.pack <$> ["#!/bin/bash", addLdLibraryPath, addPath, pyenvEnviromentVar, loadPython, shellCmd]
    liftIO $ Data.Text.IO.writeFile (encodeString lunaShellPath) fullCode

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    shelly $ do
        installPython
        installNode
        installNodeModules
        downloadLibs
        generateLunaShellScript
