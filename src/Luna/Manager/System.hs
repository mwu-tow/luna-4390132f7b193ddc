{-# LANGUAGE CPP #-}

module Luna.Manager.System where

import Prologue
import Luna.Manager.Config.Aeson

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
            deriving (Generic, Show, Eq, Ord)

data SysArch = Arch32 | Arch64        deriving (Generic, Show, Eq, Ord)
data SysDesc = SysDesc System SysArch deriving (Generic, Show, Eq, Ord)


-- === System phantoms === --

#ifdef mingw32_HOST_OS
type CurrentSystem = 'Windows
#elif linux_HOST_OS
type CurrentSystem = 'Linux
#elif darwin_HOST_OS
type CurrentSystem = 'MacOS
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
        where f = encodeShow
              g = JSON.text . encodeShow

-- Show
instance EncodeShow SysDesc where encodeShow (SysDesc s a) = encodeShow s <> encodeShow a
instance EncodeShow System  where encodeShow = Text.toLower . convert . show
instance EncodeShow SysArch where
    encodeShow = \case
        Arch32 -> "32"
        Arch64 -> "64"
