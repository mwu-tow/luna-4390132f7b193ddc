{-# LANGUAGE CPP #-}

module Luna.Manager.System.Host where

import Prologue
import Luna.Manager.Config.Aeson
import Luna.Manager.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Text           as Text


-------------------------------
-- === Operating Systems === --
-------------------------------

-- === Definition === --

data System = Linux
            | MacOS
            | Windows
            deriving (Generic, Show, Read, Eq, Ord)

data SysArch = Arch32 | Arch64        deriving (Generic, Show, Eq, Ord)
data SysDesc = SysDesc System SysArch deriving (Generic, Show, Eq, Ord)


-- === System phantoms === --

#ifdef mingw32_HOST_OS
type CurrentHost = 'Windows
#elif linux_HOST_OS
type CurrentHost = 'Linux
#elif darwin_HOST_OS
type CurrentHost = 'MacOS
#else
Running on unsupported system.
#endif


-- === Instances === --

-- JSON
instance ToJSON   System  where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   SysArch where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   SysDesc where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON System  where parseJSON  = lensJSONParse
instance FromJSON SysArch where parseJSON  = lensJSONParse
instance FromJSON SysDesc where parseJSON  = lensJSONParse
instance FromJSONKey SysDesc
instance ToJSONKey   SysDesc where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty

-- Show
instance Pretty SysDesc where
    showPretty (SysDesc s a) = showPretty s <> "." <> showPretty a
    readPretty t = case Text.splitOn "." t of
        [s,a] -> mapLeft (const "Conversion error") $ SysDesc <$> readPretty s <*> readPretty a
        _     -> Left "Incorrect system architecture format"

instance Pretty System  where
    showPretty = Text.toLower . convert . show
    readPretty = mapLeft (const "Conversion error") . tryReads . Text.toTitle

instance Pretty SysArch where
    showPretty = \case Arch32 -> "32"
                       Arch64 -> "64"
    readPretty = \case "32" -> Right Arch32
                       "64" -> Right Arch64
                       _    -> Left "Unsupported system architecture"
