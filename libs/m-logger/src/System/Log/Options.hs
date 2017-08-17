module System.Log.Options (
    module X,
    module System.Log.Options
) where

import           Options.Applicative as X
import           Prologue            hiding (switch)


optIntFlag :: Maybe String -> Char -> Int -> Int -> String -> Parser Int
optIntFlag mlongName shortName baseval defval helpmsg =
    (\sflag f -> let baselvl = if sflag then defval else baseval
                     explvl  = unsafeRead f :: Int
                     lvl     = if explvl < 0 then baselvl else explvl
                 in lvl
    )
    <$> switch    ( longName <> help helpmsg )
    <*> strOption ( short shortName <> value "-1") where
    longName = case mlongName of
        Just n  -> long n
        Nothing -> idm
