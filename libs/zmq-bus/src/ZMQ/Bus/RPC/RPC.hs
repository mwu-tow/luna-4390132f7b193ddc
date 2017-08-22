{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module ZMQ.Bus.RPC.RPC where

import           Control.Error             (ExceptT, runExceptT)
import           Control.Exception         (SomeException)
import qualified Control.Monad.Catch       as Catch
import           Control.Monad.Trans.State
import           Data.Binary               (decode, encode)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL

import           Prologue
import           System.Log.MLogger
import           ZMQ.Bus.RPC.Types         (Request, Response)



type RPC s m a = ExceptT Error (StateT s m) a

-- FIXME[PM]: to powinno byc newtypem bysmy mogli zaimplementowac instancje dla MonadState by moc uzywac "get"
--            wtedy kazdy RPC zachowuje sie jak get. Nie mozemy zrobic tego w przypadku ExceptT bo nie chcemy by KAZDY ExceptT tak sie zachowywal
--newtype RPC s m a = RPC (ExceptT Error (StateT s m) a)
--                  deriving (Functor, Monad, Foldable, Traversable, Generics, MonadEither)

--instance MonadState s (RPC s m) ...


type Error = String


data NoState = NoState
             deriving (Read, Show)


logger :: Logger
logger = getLogger $moduleName


run :: (Catch.MonadCatch m)
    => RPC s m r -> StateT s m (Either Error r)
run rpc = do
    s <- get
    let handler :: Monad m => SomeException -> m (Either String a)
        handler ex = return $ Left $ "Unhandled exception: " <> show ex
    result <- lift $ Catch.catch (Right <$> runStateT (runExceptT rpc) s) handler
    case result of
        Left   err      -> {-put s  >> -} return (Left err)
        Right (res, s') -> put s' >> return res


interceptErrors :: (MonadIO m, Monoid r) => RPC c m r -> RPC c m r
interceptErrors rpc = lift (runExceptT rpc) >>= \case
    Left err -> logger warning err >> return mempty
    Right r  -> return r


messageGet' :: B.ByteString -> Either String Request
messageGet' = return . decode . BL.fromStrict


messagePut' :: Response -> B.ByteString
messagePut' = BL.toStrict . encode
