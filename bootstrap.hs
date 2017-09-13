#!/usr/bin/env stack
-- stack --resolver lts-7.7 --install-ghc runghc --package base --package exceptions --package shelly --package text --package directory -- -hide-all-packages
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
default (T.Text)

stack = "../../tools/stack/stack"

tools = "../../tools"
supportedNodeVersion = "6.11.3"
supportedPythonVersion = "3.6.2"
-------------------
-- === Hosts === --
-------------------

-- === Definition === --

data System = Linux
            | Darwin
            | Windows
            deriving (Show, Read, Eq, Ord)


-- === System discovery === --

currentHost :: System


#ifdef linux_HOST_OS
type CurrentHost = 'Linux
currentHost      =  Linux
#elif darwin_HOST_OS
type CurrentHost = 'Darwin
currentHost      =  Darwin
#elif mingw32_HOST_OS
type CurrentHost = 'Windows
currentHost      =  Windows
#else
Running on unsupported system.
#endif

sanityCheck :: (MonadSh m, Shelly.MonadShControl m) => Shelly.FilePath -> [T.Text] -> m ()
sanityCheck command params = Shelly.silently $ do
    Shelly.errExit False $ bashLogin command params
    exit <- Shelly.lastExitCode
    when (exit /= 0) $ Shelly.errorExit (Shelly.toTextIgnore command)

puthonLibs :: [T.Text]
pythonLibs = ["requests"]

installPython :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => m ()
installPython = do
    Shelly.echo "installing python locally"
    current <- liftIO $ System.getCurrentDirectory
    let pythonFolder = tools </> "python"
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
    current <- liftIO $ System.getCurrentDirectory
    let nodeFolder = current </> tools </> "node"
    Shelly.chdir_p nodeFolder $ do
        Shelly.mkdir_p supportedNodeVersion
        case currentHost of
            Linux  -> do
                Shelly.cmd "wget" ["https://nodejs.org/download/release/latest-v6.x/node-v6.11.3-linux-x86.tar.gz"]
                Shelly.cmd  "tar" "-xpzf" "./node-v6.11.3-linux-x86.tar.gz" "--strip=1" "-C" supportedNodeVersion
                Shelly.rm "./node-v6.11.3-linux-x86.tar.gz"
            Darwin -> do
                Shelly.cmd "wget" ["https://nodejs.org/download/release/latest-v6.x/node-v6.11.3-darwin-x64.tar.gz"]
                Shelly.cmd  "tar" "-xpzf" "./node-v6.11.3-darwin-x64.tar.gz" "--strip=1" "-C" supportedNodeVersion
                Shelly.rm "./node-v6.11.3-darwin-x64.tar.gz"




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

bashLogin :: MonadSh m => Shelly.FilePath -> [T.Text] -> m T.Text
bashLogin command params = do
    Shelly.cmd "bash" ["-c", "-l", (Shelly.toTextIgnore command) `T.append` " " `T.append` T.intercalate " " params]
--
-- installNVM :: (MonadSh m, Shelly.MonadShControl m) => m ()
-- installNVM = Shelly.escaping False $ do
--     Shelly.cmd "curl" "-o- https://raw.githubusercontent.com/creationix/nvm/v0.32.1/install.sh | bash"
--     Shelly.cmd "source" "~/.bashrc"
--     sanityCheck "command" ["-v", "nvm"]
--
-- installNode ::( MonadSh m, Shelly.MonadShControl m) => m ()
-- installNode = do
--     bashLogin "nvm" ["install", supportedNodeVersion]
--     sanityCheck "node" ["--version"]


fedoraPackages :: [T.Text]
fedoraPackages = [
    "pkgconfig"
    , "zeromq-devel"
    , "ncurses-devel"
    , "zlib-devel"
    ]

brewPackages :: [T.Text]
brewPackages = [
    "pkg-config"
    , "zmq"
    ]


installBrew :: MonadSh m => m ()
installBrew = Shelly.cmd "brew" "install" brewPackages

installDnf :: MonadSh m => m ()
installDnf = Shelly.cmd "sudo" "dnf" "install" "-y" fedoraPackages

installDistroPackages :: (MonadSh m, MonadThrow m, MonadIO m) => m ()
installDistroPackages = do
    case currentHost of
        Darwin -> installBrew
        Linux  -> liftIO $ print "fupa" --installDnf

prependLocalBin :: MonadSh m => m ()
prependLocalBin = do
    homePath <- fromMaybe (error "$HOME not set") <$> Shelly.get_env "HOME"
    Shelly.prependToPath $ homePath </> ".local" </> "bin"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    shelly $ do
        return ()
        -- prependLocalBin
        -- echo "Installing dependencies"
        installDistroPackages
        -- installPython
        installNode
        --
        -- installNVM
        -- installNode
        --
        -- installBower
        -- installBrunch
        --
        -- installGHC
        -- installHaskellBins
        --
        --
        -- echo "stack setup"
        -- repoDir <- pwd
        -- chdir ("build" </> "backend") $ Shelly.cmd stack "setup"
        -- chdir "luna-studio" $ Shelly.cmd stack "setup"
