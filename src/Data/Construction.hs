module Data.Construction where



-- Constructors & Destructors

type family Destructed a

class Constructor m a where construct :: Destructed a -> m a
class Destructor  m a where destruct  :: a -> m (Destructed a)

-- Producers & Consumers

class Producer m a where produce :: m a
class Consumer m a where consume :: a -> m ()

-- Make & Destroy

class Maker     m a where make    :: m a
class Destroyer m a where destroy :: a -> m ()