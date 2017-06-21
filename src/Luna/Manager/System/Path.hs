module Luna.Manager.System.Path where

import Prologue hiding (FilePath)
import System.FilePath.Posix (pathSeparator)


type FilePath = Text
type URIPath  = Text

--TODO : expandowanie ~, . i .. w ściezkach

(</>) :: FilePath -> FilePath -> FilePath
l </> r = l <> convert pathSeparator <> r
