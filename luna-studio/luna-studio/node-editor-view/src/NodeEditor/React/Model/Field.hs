{-# LANGUAGE StrictData #-}
module NodeEditor.React.Model.Field where

import           Common.Prelude
import           NodeEditor.Event.UI        (UIEvent)


data Field r = Field { _ref :: r
                     , _content  :: Text
                     , _onAccept :: Maybe (Text -> UIEvent)
                     , _onCancel :: Maybe (Text -> UIEvent)
                     , _onEdit   :: Maybe (Text -> UIEvent)
                     }

makeLenses ''Field

instance Eq (Field r) where a == b = a ^. content == b ^. content

mk :: r -> Text -> Field r
mk r c = Field r c def def def
