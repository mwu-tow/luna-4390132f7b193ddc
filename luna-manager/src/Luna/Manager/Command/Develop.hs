module Luna.Manager.Command.Develop where

import Prologue
import Luna.Manager.Command.Options
import Luna.Manager.Network
import Luna.Manager.System.Env
import Control.Monad.Raise
import Control.Monad.State.Layered
import Luna.Manager.Shell.Shelly

run :: (MonadIO m, MonadException SomeException m, MonadGetter EnvConfig m, MonadSh m) => DevelopOpts -> m ()
run opts = do
    stackArchive <- downloadWithProgressBar "https://github.com/commercialhaskell/stack/releases/download/v1.5.1/stack-1.5.1-linux-x86_64-static.tar.gz"

    print "hello"
