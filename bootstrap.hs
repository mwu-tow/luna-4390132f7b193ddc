#!/usr/bin/env stack
-- stack --resolver lts-7.7 --install-ghc runghc --package base --package exceptions --package shelly --package text -- -hide-all-packages
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Shelly
import Control.Monad.Catch (throwM)
import Control.Exception (Exception)
import Data.Maybe (fromMaybe)
import Data.Text as T
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
default (T.Text)

supportedNodeVersion = "5.9"

sanityCheck :: Shelly.FilePath -> [T.Text] -> Sh ()
sanityCheck command params = silently $ do
    errExit False $ bashLogin command params
    exit <- lastExitCode
    when (exit /= 0) $ errorExit (toTextIgnore command)

installGHC :: Sh ()
installGHC = do
    errExit False $ cmd "stack" ["exec", "which", "--", "ghc"]
    exit <- lastExitCode
    when (exit /= 0) $ cmd "stack" "setup"

haskellBins :: [T.Text]
haskellBins = [
      "hprotoc"
    , "happy"
    , "hsc2hs"
    ]

installHaskellBins :: Sh ()
installHaskellBins = do
    cmd "stack" $ "install" : haskellBins
    sanityCheck "happy" ["--version"]

bashLogin :: Shelly.FilePath -> [T.Text] -> Sh T.Text
bashLogin command params = do
    cmd "bash" ["-c", "-l", (toTextIgnore command) `T.append` " " `T.append` T.intercalate " " params]

installNVM :: Sh ()
installNVM = escaping False $ do
    cmd "curl" "-o- https://raw.githubusercontent.com/creationix/nvm/v0.32.1/install.sh | bash"
    cmd "source" "~/.bashrc"
    sanityCheck "command" ["-v", "nvm"]

installNode :: Sh ()
installNode = do
    bashLogin "nvm" ["install", supportedNodeVersion]
    sanityCheck "node" ["--version"]

installBower :: Sh ()
installBower = do
   bashLogin "npm" ["install", "-g", "bower"]
   sanityCheck "bower" ["--version"]

installBrunch :: Sh ()
installBrunch = do
    bashLogin "npm" ["install", "-g", "brunch@1.8.5"]
    sanityCheck "brunch" ["--version"]

fedoraPackages :: [T.Text]
fedoraPackages = [
      "supervisor"
    , "pkgconfig"
    , "zeromq-devel"
    , "ncurses-devel"
    , "zlib-devel"
    ]

brewPackages :: [T.Text]
brewPackages = [
      "supervisor"
    , "pkg-config"
    , "zmq"
    ]

data Distro = Fedora

data OS = MacOS | Linux Distro

detectOS :: Sh OS
detectOS = do
    uname <- cmd "uname"
    case uname of
        "Darwin" -> return MacOS
        _        -> detectLinuxDistro

data UnsupportedLinuxDistribution = UnsupportedLinuxDistribution
    deriving (Show, Exception)

detectLinuxDistro :: Sh OS
detectLinuxDistro = do
    distro <- cmd "lsb_release" "-si"
    case distro of
        "Fedora\n" -> return $ Linux Fedora
        _          -> throwM UnsupportedLinuxDistribution

installBrew :: Sh ()
installBrew = cmd "brew" "install" brewPackages

installDnf :: Sh ()
installDnf = cmd "sudo" "dnf" "install" "-y" fedoraPackages

installDistroPackages :: Sh ()
installDistroPackages = do
    os <- detectOS
    case os of
        MacOS        -> installBrew
        Linux Fedora -> installDnf

prependLocalBin :: Sh ()
prependLocalBin = do
    homePath <- fromMaybe (error "$HOME not set") <$> get_env "HOME"
    prependToPath $ homePath </> ".local" </> "bin"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    shelly $ do
        prependLocalBin
        echo "Installing dependencies"
        installDistroPackages

        installNVM
        installNode

        installBower
        installBrunch

        installGHC
        installHaskellBins

        echo "Compiling project"

        echo "Updating submodules"
        cmd "git" ["submodule", "update", "--init"]

        echo "stack setup"
        repoDir <- pwd
        chdir ("build" </> "backend") $ cmd "stack" "setup"
        chdir "nodelab" $ cmd "stack" "setup"

        chdir "nodelab" $ do
            cmd "npm" "install"
            cmd "bower" "install" "--allow-root"

        chdir ("build" </> "backend") $ cmd "stack" "build" "--copy-bins" "--fast"
        chdir "nodelab" $ cmd "brunch" "build"
