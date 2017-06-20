module Luna.Manager.Component.Pretty where

import Prologue


class Pretty a where
    showPretty :: a -> Text
    readPretty :: Text -> Either Text a

    default showPretty :: Show a => a -> Text
    default readPretty :: Read a => Text -> Either Text a
    showPretty = convert . show
    readPretty = mapLeft (convert . ("Read error. Remaining part: " <>)) . tryReads

instance Pretty Text where
    showPretty = id
    readPretty = Right
