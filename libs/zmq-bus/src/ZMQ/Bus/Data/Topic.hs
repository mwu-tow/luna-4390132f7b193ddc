module ZMQ.Bus.Data.Topic where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import qualified Data.List             as List
import qualified Data.String.Utils     as Utils

import           Prologue



type Topic = String


toByteString :: Topic -> ByteString
toByteString = pack


fromByteString :: ByteString -> Topic
fromByteString = unpack


separator :: Topic
separator = "."


error :: Topic
error = "error"


status :: Topic
status = "status"


update :: Topic
update = "update"


request :: Topic
request = "request"


base :: Topic -> Topic
base = List.intercalate separator . unsafeInit . Utils.split separator

-- FIXME[WD->PM]: MONOIDS!!!
-- | Replace last part of topic
(/+) :: Topic -> Topic -> Topic
topic /+ type_ = respond topic type_


respond :: Topic -> Topic -> Topic
respond topic type_ =
    (List.intercalate separator . flip (<>) [type_] . unsafeInit . Utils.split separator) topic


isRequest :: Topic -> Bool
isRequest = List.isSuffixOf request
