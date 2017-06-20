module Luna.Manager.Component.Version where

import Prologue
import Control.Lens.Aeson
import Luna.Manager.Component.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Text           as Text


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

instance Pretty VersionTag where
    showPretty = \case
        Alpha -> "alpha"
        Beta  -> "beta"
        RC i  -> "rc" <> convert (show i)
    readPretty = \case
        "alpha" -> Right Alpha
        "beta"  -> Right Beta
        s       -> case Text.take 2 s of
            "rc" -> RC <$> mapLeft (const "Conversion error") (tryReads @String $ Text.drop 2 s)
            _    -> Left "Incorrect version tag format"

instance Pretty Version where
    showPretty (Version major minor build tag) = intercalate "." (map (convert . show) [major, minor, build])
                                              <> maybe "" (("." <>) . showPretty) tag
    readPretty t = case Text.splitOn "." t of
        [major, minor, build, tag] -> mapLeft (const "Conversion error") $ Version <$> tryReads major <*> tryReads minor <*> tryReads build <*> (Just <$> mapLeft convert (readPretty tag))
        [major, minor, build]      -> mapLeft (const "Conversion error") $ Version <$> tryReads major <*> tryReads minor <*> tryReads build <*> pure Nothing
        _                          -> Left "Incorrect version format"

makeLenses ''Version
makeLenses ''VersionTag

instance ToJSON   Version    where toEncoding = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON   VersionTag where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON Version    where parseJSON  = lensJSONParse
instance FromJSON VersionTag where parseJSON  = lensJSONParse
instance FromJSONKey Version
instance ToJSONKey   Version where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty
