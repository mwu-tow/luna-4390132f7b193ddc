module Luna.Manager.Component.Version where

import Prologue
import Control.Lens.Aeson
import Luna.Manager.Component.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON)
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
                | RC !Word64
                deriving (Generic, Show, Eq, Ord)

data Nightly = Nightly !Word64 deriving (Generic, Show, Eq, Ord)

data Version = Version { _major   :: !Word64
                       , _minor   :: !Word64
                       , _build   :: !Word64
                       , _tag     :: !(Maybe VersionTag)
                       , _nightly :: !(Maybe Nightly)
                       } deriving (Generic, Show, Eq, Ord)

makeLenses ''Version
makeLenses ''Nightly
makeLenses ''VersionTag

-- === Instances === --

instance Pretty VersionTag where
    showPretty = \case
        Alpha   -> "alpha"
        Beta    -> "beta"
        RC i    -> "rc" <> convert (show i)
    readPretty = \case
        "alpha"   -> Right Alpha
        "beta"    -> Right Beta
        s         -> case Text.take 2 s of
            "rc" -> RC <$> mapLeft (const "Conversion error") (tryReads @String $ Text.drop 2 s)
            _    -> Left "Incorrect version tag format"

instance Pretty Nightly where
    showPretty (Nightly i) = "nightly" <> convert (show i)
    readPretty s = case Text.take 7 s of
        "nightly" -> Nightly <$> mapLeft (const "Conversion error") (tryReads @String $ Text.drop 7 s)
        _ -> Left "Incorrect version tag format"

instance Pretty Version where
    showPretty (Version major minor build tag nightly) = intercalate "." (map (convert . show) [major, minor, build])
                                              <> maybe "" (("." <>) . showPretty) tag <> maybe "" (("." <>) . showPretty) nightly
    readPretty t = case Text.splitOn "." t of
        [ma, mi, b, t, n] -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> (Just <$> mapLeft convert (readPretty t)) <*> (Just <$> mapLeft convert (readPretty n))
        [ma, mi, b, n]    -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> pure Nothing <*> (Just <$> mapLeft convert (readPretty n))
        [ma, mi, b, t]    -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> (Just <$> mapLeft convert (readPretty t)) <*> pure Nothing
        [ma, mi, b]       -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> pure Nothing <*> pure Nothing
        _                 -> Left "Incorrect version format"
        where cerr = mapLeft (const "Conversion error")

-- JSON
instance ToJSON      Version    where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      Nightly    where toEncoding  = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON      VersionTag where toEncoding  = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON    Version    where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    Nightly    where parseJSON   = lensJSONParse
instance FromJSON    VersionTag where parseJSON   = lensJSONParse
instance FromJSONKey Version    where fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   Version    where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty
