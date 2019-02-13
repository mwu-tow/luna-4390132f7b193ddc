module Luna.Manager.System.Info where

import Prologue
import Data.Version (showVersion)
import qualified Paths_luna_manager as Paths

-----------------------------------------
-- === General manager information === --
-----------------------------------------

synopsis :: IsString s => s
synopsis = "Luna Manager provides install / uninstall / update and maintain utilities over all Luna related ecosystem components."

version :: IsString s => s
version = fromString $ showVersion Paths.version
