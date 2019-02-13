module Luna.Manager.Network where

import Prologue hiding (FilePath, fromText)

import qualified Control.Exception.Safe     as Exception
import qualified Data.ByteString.Char8      as ByteStringChar (unpack)
import qualified Data.ByteString.Lazy.Char8 as ByteStringL
import qualified Luna.Manager.Logger        as Logger
import qualified Network.HTTP.Conduit       as HTTP
import qualified Network.URI                as URI

import Control.Monad.Raise
import Control.Monad.State.Layered
import Control.Monad.Trans.Resource      (MonadBaseControl, runResourceT)
import Data.Conduit                      (($$+-), ($=+))
import Data.Conduit.Binary               (sinkFile)
import Filesystem.Path.CurrentOS
import Luna.Manager.Command.Options      (Options, guiInstallerOpt)
import Luna.Manager.Gui.DownloadProgress (Progress (..))
import Luna.Manager.Shell.ProgressBar
import Luna.Manager.Shell.Shelly         (MonadSh, MonadShControl)
import Luna.Manager.System.Env
import Luna.Manager.System.Path
import Network.HTTP.Conduit              (httpLbs)
import Network.HTTP.Types                (hContentLength)
import System.Directory                  (doesFileExist)

-- === Errors === --

data DownloadException = DownloadException Text SomeException deriving (Show)
instance Exception DownloadException where
    displayException (DownloadException file exception) = "Couldn't download file: " <> convert file <> " because of: "  <> displayException exception

data DownloadError = DownloadError { uriPath :: URIPath } deriving (Show)
instance Exception DownloadError where
    displayException (DownloadError p) = "Download Error: cannot read file: " <> show p

downloadError :: URIPath -> SomeException
downloadError = toException . DownloadError


-- === Utils === --

takeFileNameFromURL :: URIPath -> Maybe Text
takeFileNameFromURL url = convert <$> name where
    name = maybeLast . URI.pathSegments =<< URI.parseURI (convert url)

type MonadNetwork m = (MonadIO m, MonadGetters '[Options, EnvConfig] m, MonadException SomeException m, MonadSh m, MonadShControl m, MonadCatch m, MonadThrow m,  MonadBaseControl IO m)

fileExists :: MonadIO m => FilePath -> m Bool
fileExists = liftIO . doesFileExist . encodeString

downloadFromURL :: MonadNetwork m => URIPath -> Text -> m FilePath
downloadFromURL address info = do
    let go = withJust (takeFileNameFromURL address) $ \name -> do
            Logger.log $ info <>" (" <> address <> ")"
            dest    <- (</> (fromText name)) <$> getDownloadPath
            unlessM (fileExists dest) $ do
                manager <- newHTTPManager
                request <- HTTP.parseUrlThrow (convert address)
                resp    <- httpLbs request manager
                liftIO $ ByteStringL.writeFile (encodeString dest) $ HTTP.responseBody resp
            return dest
    go `Exception.catchAny` \e -> throwM (DownloadException address e)

newHTTPManager :: MonadIO m => m HTTP.Manager
newHTTPManager = liftIO . HTTP.newManager $ HTTP.tlsManagerSettings { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 30000000}

downloadWithProgressBar  :: MonadNetwork m => URIPath -> m FilePath
downloadWithProgressBar address = do
    Logger.log $ "Downloading: " <> address
    tmp <- getTmpPath
    downloadWithProgressBarTo address tmp

downloadWithProgressBarTo :: MonadNetwork m => URIPath -> FilePath -> m FilePath
downloadWithProgressBarTo address dstPath = Exception.handleAny (\e -> throwM (DownloadException address e)) $  do
    guiInstaller <- guiInstallerOpt
    req          <- HTTP.parseRequest (convert address)
    manager      <- newHTTPManager
    -- Start the request
    runResourceT $ withJust (takeFileNameFromURL address) $ \name -> do
        let dstFile = dstPath </> (fromText name)
        res <- HTTP.http req manager
        -- Get the Content-Length and initialize the progress bar
        cl <- tryJust (downloadError address) $ lookup hContentLength (HTTP.responseHeaders res)
        let pgTotal  = read (ByteStringChar.unpack cl)
            pg       = ProgressBar 50 0 pgTotal
            progress = Progress 0 pgTotal
        -- Consume the response updating the progress bar
        if guiInstaller then
            HTTP.responseBody res $=+ updateProgress progress $$+- sinkFile (encodeString dstFile)
        else do
            HTTP.responseBody res $=+ updateProgressBar pg    $$+- sinkFile (encodeString dstFile)
            putStrLn "Download completed!"
        return dstFile
