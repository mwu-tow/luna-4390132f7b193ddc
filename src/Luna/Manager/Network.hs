module Luna.Manager.Network where

import Prologue hiding (FilePath)

import Luna.Manager.System.Path
import Luna.Manager.System.Env
import Luna.Manager.Shell.ProgressBar

import Control.Monad.Raise
import Control.Monad.State.Layered
import qualified Network.HTTP.Conduit       as HTTP
import           Network.HTTP.Conduit       (httpLbs)
import qualified Network.URI                as URI
import qualified Data.ByteString.Lazy.Char8 as ByteStringL
import qualified Control.Exception.Base     as Exception

import Control.Monad.Trans.Resource (runResourceT)

import Data.Conduit (($$+-),($=+))
import Data.Conduit.List (sinkNull)
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Types (hContentLength)
import qualified Data.ByteString.Char8 as ByteStringChar (unpack, writeFile)
import qualified Data.Text as Text


-- === Errors === --

data DownloadError = DownloadError deriving (Show)
instance Exception DownloadError

downloadError :: SomeException
downloadError = toException DownloadError


-- === Utils === --

takeFileNameFromURL :: URIPath -> Maybe Text
takeFileNameFromURL url = convert <$> name where
    name = maybeLast . URI.pathSegments =<< URI.parseURI (convert url)

type MonadNetwork m = (MonadIO m, MonadGetter EnvConfig m, MonadException SomeException m)

downloadFromURL :: MonadNetwork m => URIPath -> m FilePath
downloadFromURL address = tryJust downloadError =<< go where
    go = withJust (takeFileNameFromURL address) $ \name -> do
        putStrLn $ "Downloading repository configuration file (" <> convert address <> ")"
        dest    <- (</> name) <$> getDownloadPath
        manager <- newHTTPManager
        request <- tryRight' $ HTTP.parseRequest (convert address)
        resp    <- tryRight' @SomeException =<< liftIO (Exception.try $ httpLbs request manager)
        liftIO $ ByteStringL.writeFile (convert dest) $ HTTP.responseBody resp
        return (Just dest)


newHTTPManager :: MonadIO m => m HTTP.Manager
newHTTPManager = liftIO . HTTP.newManager $ HTTP.tlsManagerSettings { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 5000000}

downloadWithProgressBar :: (MonadIO m, MonadException SomeException m) => URIPath -> FilePath -> m FilePath
downloadWithProgressBar address dstPath = do
    req <- tryRight' $ HTTP.parseRequest (convert address)
    manager <- newHTTPManager
    tryRight' @SomeException <=< liftIO . Exception.try . runResourceT $ do
    -- Start the request
        withJust (takeFileNameFromURL address) $ \name -> do
            let dstFile = dstPath </> name
            res <- HTTP.http req manager
            -- Get the Content-Length and initialize the progress bar
            let Just cl = lookup hContentLength (HTTP.responseHeaders res)
                pgTotal = read (ByteStringChar.unpack cl)
                pg      = ProgressBar 50 0 pgTotal
            -- Consume the response updating the progress bar
            HTTP.responseBody res $=+ updateProgress pg $$+- sinkFile (convert dstFile)
            putStrLn "Download completed!"
            return dstFile
