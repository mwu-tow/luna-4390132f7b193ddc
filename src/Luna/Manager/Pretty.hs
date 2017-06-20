module Luna.Manager.Pretty where

import Prologue


class Pretty a where
    showPretty :: a -> Text
    readPretty :: Text -> Either Text a

    default showPretty :: Show a => a -> Text
    default readPretty :: Read a => Text -> Either Text a
    showPretty   = convert . show
    readPretty t = case reads (convert t) of
        [(a,[])]    -> Right a
        ((_,s):_) -> Left . convert $ "Read error. Remaining part: '" <> s <> "'"
