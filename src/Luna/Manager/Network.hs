module Luna.Manager.Network where

import Prologue hiding (FilePath)

import Luna.Manager.System.Path
import Luna.Manager.System.Config

import Control.Monad.Raise
import Control.Monad.State.Layered
import qualified Network.HTTP.Conduit       as HTTP
import           Network.HTTP.Conduit       (httpLbs)
import qualified Network.URI                as URI
import qualified Data.ByteString.Lazy.Char8 as ByteStringL
import qualified Control.Exception.Base     as Exception


-- === Errors === --

data DownloadError = DownloadError deriving (Show)
instance Exception DownloadError

downloadError :: SomeException
downloadError = toException DownloadError


-- === Utils === --

takeFileNameFromURL :: URIPath -> Maybe Text
takeFileNameFromURL url = convert <$> name where
    name = maybeLast . URI.pathSegments =<< URI.parseURI (convert url)

type MonadNetwork m = (MonadIO m, MonadGetter SystemConfig m, MonadException SomeException m)

downloadFromURL :: MonadNetwork m => URIPath -> m FilePath
downloadFromURL address = tryJust downloadError =<< go where
    go = withJust (takeFileNameFromURL address) $ \name -> do
        putStrLn $ "Downloading Luna repository configuration file (" <> convert address <> ")"
        dest    <- (</> name) <$> getDownloadPath
        manager <- newHTTPManager
        request <- tryRight' $ HTTP.parseRequest (convert address)
        resp    <- tryRight' @SomeException =<< liftIO (Exception.try $ httpLbs request manager)
        liftIO $ ByteStringL.writeFile (convert dest) $ HTTP.responseBody resp
        return (Just dest)


newHTTPManager :: MonadIO m => m HTTP.Manager
newHTTPManager = liftIO . HTTP.newManager $ HTTP.tlsManagerSettings { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 5000000}
