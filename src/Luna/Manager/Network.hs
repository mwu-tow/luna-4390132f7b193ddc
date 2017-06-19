module Luna.Manager.Network where

import Prologue hiding (FilePath)

import Luna.Manager.System.Path
import Luna.Manager.System.Config

import Control.Monad.Raise
import Control.Monad.State.Layered
import qualified Network.HTTP.Conduit       as HTTP
import qualified Network.URI                as URI
import qualified Data.ByteString.Lazy.Char8 as ByteStringL



-- === Errors === --

data DownloadError = DownloadError deriving (Show)
instance Exception DownloadError

downloadError :: SomeException
downloadError = toException DownloadError


-- === Utils === --

takeFileNameFromURL :: URIPath -> Maybe Text
takeFileNameFromURL url = convert <$> name where
    name = maybeTail . URI.pathSegments =<< URI.parseURI (convert url)

downloadFromURL :: (MonadIO m, MonadGetter SystemConfig m, MonadException SomeException m) => URIPath -> m FilePath
downloadFromURL address = tryJust downloadError =<< go where
    go = withJust (takeFileNameFromURL address) $ \name -> do
        path <- (</> name) <$> getDownloadPath
        liftIO $ ByteStringL.writeFile (convert path) =<< HTTP.simpleHttp (convert address)
        return (Just path)
