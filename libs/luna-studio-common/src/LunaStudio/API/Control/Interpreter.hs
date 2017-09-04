module LunaStudio.API.Control.Interpreter where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import qualified LunaStudio.API.Topic    as T
import           Prologue


data Request = Start
             | Pause
             | Reload
             deriving (Eq, Generic, Show)

data Update = Update Text deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Update
instance Binary Request
instance NFData Request
instance Binary Update
instance NFData Update


type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()

topicPrefix :: T.Topic
topicPrefix = "empire.control.interpreter"

instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update              where topic _ = topicPrefix <> T.update
