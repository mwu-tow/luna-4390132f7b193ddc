module Main where

import Empire.Prelude

import qualified Language.Haskell.TH as TH
import qualified Spec

import Luna.Package.Structure.Name (lunaRootEnv)
import System.Directory            (canonicalizePath, getCurrentDirectory)
import System.Environment          (lookupEnv, setEnv)
import System.FilePath             (takeDirectory, (</>))
import Control.Exception (bracket)
import Test.Hspec (hspec)


main :: IO ()
main = bracket setTestEnv unsetTestEnv (\_ -> hspec Spec.spec) where
    setTestEnv = do
        let testMainHsPath = $(do
                dir      <- TH.runIO getCurrentDirectory
                fileName <- TH.loc_filename <$> TH.location
                TH.litE . TH.stringL $ dir </> fileName)
            envDirName  = "env"
            repoDirPath = takeDirectory testMainHsPath </> "../../.."
            envDirPath  = repoDirPath </> envDirName
        userLunaRoot   <- fromMaybe mempty <$> lookupEnv lunaRootEnv
        mockedLunaRoot <- canonicalizePath envDirPath
        setEnv lunaRootEnv mockedLunaRoot
        pure userLunaRoot
    unsetTestEnv = setEnv lunaRootEnv

