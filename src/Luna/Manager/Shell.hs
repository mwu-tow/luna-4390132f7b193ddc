module Luna.Manager.Shell where

import Prologue



-------------------------
-- === Shell utils === --
-------------------------

askQuestion :: MonadIO m => (Text -> Either Text a) -> Maybe Text -> Text -> m a
askQuestion validator defAns question = do
    let defAnsSfx    = maybe "" (\s -> "[" <> s <> "]") defAns
        questionLine = question <> defAnsSfx <> ": "
    putStrLn $ convert questionLine
    resp <- liftIO $ convert <$> getLine
    let resp' = maybe resp (\a -> if resp == "" then a else resp) defAns
    case validator resp' of
        Left e -> do
            putStrLn $ "Wrong answer:" <> convert e
            askQuestion validator defAns question
        Right a -> return a
