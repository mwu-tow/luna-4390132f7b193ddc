module Luna.Manager.System.Path where

import Prologue hiding (FilePath, null, fromText)

import Luna.Manager.System.Env

import qualified Filesystem.Path as Path
import Filesystem.Path (FilePath, null)
import Filesystem.Path.CurrentOS (fromText, encodeString)
import Control.Monad.Raise

type URIPath  = Text

instance Convertible Text FilePath where
    convert = fromText

expand :: MonadIO m => FilePath -> m FilePath
expand path = do
    if null path
        then return path
        else do
            let dirs = Path.splitDirectories path
                fstEl = head dirs
            home <- getHomePath
            current <- getCurrentPath
            case encodeString fstEl of
                "~/"      -> return $ Path.concat $ home : (tail dirs)
                "./"      -> return $ Path.concat $ current : (tail dirs)
                "../"     -> return $ Path.concat $ (Path.parent current) : (tail dirs)
                otherwise -> return path
