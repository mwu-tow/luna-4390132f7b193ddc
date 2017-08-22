module Empire.Prelude (module X, nameToString, pathNameToString, stringToName, (<?!>)) where

import qualified Data.Convert              as Convert
import qualified OCI.IR.Name.Qualified     as IR
import Prologue as X hiding (TypeRep, p, r, s, (|>), return, liftIO, fromMaybe, when)
import Control.Monad       as X (return, when)
import Control.Monad.Trans as X (liftIO)
import Data.List           as X (sort)
import Data.Maybe          as X (fromMaybe)

infixr 0 <?!>
(<?!>) :: (Exception e, MonadThrow m) => m (Maybe a) -> e -> m a
m <?!> e = m >>= maybe (throwM e) pure

nameToString :: IR.Name -> String
nameToString = Convert.convert

pathNameToString :: IR.QualName -> String
pathNameToString = nameToString . Convert.convert

stringToName :: String -> IR.Name
stringToName = Convert.convert
