module LunaStudio.API.Graph.Paste where

import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Connection    (Connection)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (ExpressionNode)
import           LunaStudio.Data.Position      (Position)
import           Prologue

data Request = Request { _location      :: GraphLocation
                       , _position      :: Position
                       , _clipboardData :: String
                       } deriving (Eq, Generic, NFData, Show)

data Result  = Result  { _nodes       :: [ExpressionNode]
                       , _connections :: [Connection]
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance Binary Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.paste"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
