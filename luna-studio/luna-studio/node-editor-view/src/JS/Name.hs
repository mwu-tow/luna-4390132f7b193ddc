module JS.Name where

import           Common.Prelude

toName :: Show a => a -> JSString
toName = convert . intercalate "-" . toWords . map toLower . show where
    toWords [] = []
    toWords t  = let (w, suffix) = nextWord t in if null w then toWords suffix else w : toWords suffix
    nextWord = span isAlphaNum . dropWhile (not . isAlphaNum)
