module Luna.Manager.System.Path where

import Prologue hiding (FilePath)
import System.FilePath.Posix (pathSeparator)


type FilePath = Text
type URIPath  = Text

(</>) :: FilePath -> FilePath -> FilePath
l </> r = l <> convert pathSeparator <> r
