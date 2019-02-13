module ZMQ.Bus.Data.Prefix where

import           Control.Arrow      (first)
import           Prologue
import           ZMQ.Bus.Data.Topic (Topic)



type Prefix = String


prefixify :: Prefix -> Topic -> Topic
prefixify prefix topic = if null prefix
    then topic
    else prefix <> "." <> topic


prefixifyTopics :: Prefix -> [(Topic, a)] -> [(Topic, a)]
prefixifyTopics prefix = map (first $ prefixify prefix)
