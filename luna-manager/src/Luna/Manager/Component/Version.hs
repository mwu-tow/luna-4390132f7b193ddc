module Luna.Manager.Component.Version where

import Prologue
import Control.Lens.Aeson
import Luna.Manager.Component.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Text           as Text
import Control.Error.Util (hush)

------------------------
-- === Versioning === --
------------------------

-- === Definition === --

data VersionInfo = VersionInfo { _nightlyNumber :: !Word64
                               , _buildNumber   :: !(Maybe Word64)
                               } deriving (Generic, Show, Eq, Ord)

data Version = Version { _major :: !Word64
                       , _minor :: !Word64
                       , _info  :: !(Maybe VersionInfo)
                       } deriving (Generic, Show, Eq, Ord)

makeLenses ''Version
makeLenses ''VersionInfo

cShow = convert . show

-- === Instances === --

instance Pretty VersionInfo where
    showPretty (VersionInfo nn bn) = cShow nn <> maybe "" (("." <>) . cShow) bn
    readPretty s = case Text.splitOn "." s of
        (nn:bn:_) -> mapLeft convert $ VersionInfo <$> tryReads nn <*> Right (hush (tryReads bn))
        [nn]      -> mapLeft convert $ VersionInfo <$> tryReads nn <*> pure Nothing
        _         -> Left "Incorrect version info format. Expected: <nightly_number>[.<build_number>]"

instance Pretty Version where
    showPretty (Version ma mi info) = intercalate "." (map (convert . show) [ma, mi])
                                    <> maybe "" (("." <>) . showPretty) info
    readPretty t = case Text.splitOn "." t of
        [ma, mi]   -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> pure Nothing
        (ma:mi:nn) -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> Right (hush $ readPretty $ Text.intercalate "." nn)
        _           -> Left "Incorrect version format"
        where cerr = mapLeft convert

-- JSON
instance ToJSON      Version     where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      VersionInfo where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance FromJSON    Version     where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    VersionInfo where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSONKey Version     where fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   Version     where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty
