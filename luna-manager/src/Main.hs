module Main where

import Prologue
import Control.Monad.Raise
import Luna.Manager.Command.Options (evalOptionsParserT)
import Luna.Manager.Command         (chooseCommand)



main :: IO ()
main = handleAll handleTopLvlError $ evalOptionsParserT chooseCommand 

handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e
