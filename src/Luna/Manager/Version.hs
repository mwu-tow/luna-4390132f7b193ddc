module Luna.Manager.Version where

import Prologue
import Luna.Manager.Config.Aeson
import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON

------------------------
-- === Versioning === --
------------------------

-- === Definition === --

data VersionTag = Alpha
                | Beta
                | RC Word64
                deriving (Generic, Show, Eq, Ord)

data Version = Version { _major :: !Word64
                       , _minor :: !Word64
                       , _build :: !Word64
                       , _tag   :: !(Maybe VersionTag)
                       } deriving (Generic, Show, Eq, Ord)

-- === Instances === --

instance EncodeShow VersionTag where
    encodeShow = \case
        Alpha -> "alpha"
        Beta  -> "beta"
        RC i  -> "rc" <> convert (show i)

instance EncodeShow Version where
    encodeShow (Version major minor build tag) = intercalate "." (map (convert . show) [major, minor, build])
                                              <> maybe "" (("." <>) . encodeShow) tag


makeLenses ''Version
makeLenses ''VersionTag

instance ToJSON   Version    where toEncoding = JSON.toEncoding . encodeShow; toJSON = JSON.toJSON . encodeShow
instance ToJSON   VersionTag where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON Version    where parseJSON  = lensJSONParse
instance FromJSON VersionTag where parseJSON  = lensJSONParse
instance FromJSONKey Version
instance ToJSONKey   Version where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = encodeShow
              g = JSON.text . encodeShow
