{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}

module Main where

-- import Prelude hiding (Either, Left, Right)
-- import qualified Prelude as P
-- import Control.Monad.Trans
-- import Control.DeepSeq (NFData, force)
--
-- import Data.Monoid
-- import Criterion.Main
-- import Control.Monad.State.Layered
-- import qualified Data.Text as Text
-- import           Data.Text (Text)
-- import Control.Monad.Identity
-- import GHC.Generics (Generic)
-- import GHC.IO          as X (evaluate)
-- import Data.String (fromString)
-- import Control.Lens
-- import Data.Void
--
-- import Unsafe.Coerce (unsafeCoerce)
--
-- import qualified Text.Megaparsec as Megaparsec
-- import qualified Text.Megaparsec.Char as Megaparsec
--

import Prelude as P hiding (Either, Left, Right)
import qualified Data.Text as Text
import Data.Text (Text)
import Criterion.Main
import Control.Monad.State.Layered hiding ((.))
import qualified Control.Monad.State.Strict as S
import Control.Monad.Codensity
import qualified Control.Monad.State.CPS as CPS
import Data.Word
import Control.Monad.Identity
import GHC.IO          as X (evaluate)
import Control.DeepSeq
import System.TimeIt
import System.Environment (getArgs)
import System.IO (stdout, hSetBuffering, BufferMode(..))
import GHC.Generics (Generic)
import Control.Monad.Trans
import Control.Applicative
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
-- import Control.Monad.Trans.Either
import Data.Void



------------------------
-- === Primitives === --
------------------------

-- === Either === --

data    Either  e   a = Left !e | Right !a deriving (Eq, Generic, Ord, Read, Show, Functor)
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (EitherT e m) where
  fmap f = \ !a -> EitherT $ liftM (fmap f) $ runEitherT a ; {-# INLINE fmap #-}

instance Monad m => Applicative (EitherT e m) where
  pure  = undefined ; {-# INLINE pure #-}
  (<*>) = undefined ; {-# INLINE (<*>) #-}

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a) ; {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return $ Left l
      Right r -> runEitherT $ k r
  {-# INLINE (>>=) #-}
  fail a = EitherT $ fail a ; {-# INLINE fail #-}

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right ; {-# INLINE lift #-}

instance (NFData l, NFData r) => NFData (Either l r)


type family Item a



-- data T a b = T !a !b deriving (Show, Functor, Generic)
-- instance (NFData a, NFData b) => NFData (T a b)
--
-- tsnd :: T a b -> b
-- tsnd (T _ b) = b ; {-# INLINE tsnd #-}


------------------------
-- === FailParser === --
------------------------

-- === Definition === --

newtype FailParser e m a = FailParser { fromFailParser :: EitherT e m a } deriving (Functor, Applicative, Monad, MonadTrans)

-- === Running === --

runFailParser :: forall e m a. FailParser e m a -> m (Either e a)
runFailParser f = runEitherT $ fromFailParser f ; {-# INLINE runFailParser #-}


-- === MonadFailedParser === --

class Monad m => MonadFailedParser m where
    failed :: m a

instance {-# OVERLAPPABLE #-} (MonadFailedParser m, MonadTrans t, Monad (t m))
      => MonadFailedParser (t m) where
    failed = lift failed ; {-# INLINE failed #-}

instance Monad m => MonadFailedParser (FailParser () m) where
    failed = FailParser $ EitherT $ return $ Left () ; {-# INLINE failed #-}

instance (Monad m, MonadGetter Bool m) => MonadPlus   (FailParser e m)
instance (Monad m, MonadGetter Bool m) => Alternative (FailParser e m) where
    l <|> r = FailParser $ EitherT $ do
        lv <- runFailParser l
        prog <- get @Bool
        if prog then return lv else case lv of
            Right _ -> return lv
            Left e  -> runFailParser r


    {-# INLINE (<|>) #-}
-- intance Alternat
--
-- instance Monad m => MonadFailedParser (ResultT () m) where
--     failed = ResultT $ return $ Error ()
--
--
-- -- === ProgressMonad === --
--
-- class ProgressMonad m where
--     returnProgressed :: forall a. a -> m a
--
-- instance Monad m => ProgressMonad (FailParser m) where
--     returnProgressed a = failParser $ return $ Right $ T True a ; {-# INLINE returnProgressed #-}






--------------------
-- === Stream === --
--------------------

newtype Stream s = Stream s deriving (Show, Functor, Foldable, Traversable)

class IsStream s where
    uncons :: s -> Maybe (Item s, s)

type instance Item (Stream s) = Item s
instance IsStream s => IsStream (Stream s) where
    uncons (Stream !s) = (fmap.fmap) Stream $ uncons s ; {-# INLINE uncons #-}

type instance Item Text = Char
instance IsStream Text where
    uncons = Text.uncons ; {-# INLINE uncons #-}

type instance Item [a] = a
instance IsStream String where
    uncons = \case
        [] -> Nothing
        ((!s) : (!ss)) -> Just (s, ss)
    {-# INLINE uncons #-}




eval :: NFData a => a -> IO a
eval = evaluate . force ; {-# INLINE eval #-}


mtlIncState :: S.MonadState Int m => m ()
mtlIncState = S.modify (1+) ; {-# INLINE mtlIncState #-}
{-# SPECIALIZE mtlIncState :: S.State Int () #-}


-- | incLoop transformation needed because of bug https://ghc.haskell.org/trac/ghc/ticket/14062
incLoop2 :: (MonadStates '[Int, Bool] m, MonadState Stream m, IsStream (StateData Stream m), Item (StateData Stream m) ~ Char, MonadFailedParser m, Alternative m) => Int -> m Int
incLoop2 n = repeatM step n >> get @Int ; {-# INLINE incLoop2 #-}

step :: (MonadStates '[Int, Bool] m, MonadState Stream m, IsStream (StateData Stream m), Item (StateData Stream m) ~ Char, MonadFailedParser m, Alternative m) => m Char
step = do
    !s <- get @Stream
    case uncons s of
        Just (!a,!s') -> if a == 'a' then put @Stream s' >> put @Bool True >> return a else failed -- >> modify_ @Int (1+)
        Nothing       -> failed
{-# INLINE step #-}


repeatM2 :: (Monad m, Alternative m) => m Char -> Int -> m [Char]
repeatM2 f = go where
    go 0 = pure []
    go i = (:) <$> f <*> (go (i - 1) <|> pure [])
{-# INLINE repeatM2 #-}

repeatM :: Monad m => m a -> Int -> m ()
repeatM f = go where
    go 0 = pure ()
    go i = f >> go (i - 1)
{-# INLINE repeatM #-}

pureInc :: Int -> Int -> Int
pureInc !a !i = case i of
    0 -> a
    _ -> pureInc (a + 1) (i - 1)

pureInc2 :: Int -> Word -> Int -> (Int, Word)
pureInc2 !a !b i = case i of
    0 -> (a,b)
    _ -> pureInc2 (a + 1) (b + 1) (i - 1)

sInt :: Functor m => StateT Int    m a -> m a
sWrd :: Functor m => StateT Word   m a -> m a
sStr :: Functor m => StateT String m a -> m a
sChr :: Functor m => StateT Char   m a -> m a
sTup :: Functor m => StateT ()     m a -> m a
sInt = flip (evalStateT @Int)    0   ; {-# INLINE sInt #-}
sWrd = flip (evalStateT @Word)   0   ; {-# INLINE sWrd #-}
sStr = flip (evalStateT @String) ""  ; {-# INLINE sStr #-}
sChr = flip (evalStateT @Char)   'x' ; {-# INLINE sChr #-}
sTup = flip (evalStateT @())     ()  ; {-# INLINE sTup #-}

test :: forall a. (IsStream a, Item a ~ Char) => a -> Int -> Either () Int
test t = runIdentity . flip (evalStateT @Bool) False . runFailParser . sInt . flip (evalStateT @(Stream a)) (Stream t :: Stream a) . incLoop2 ; {-# INLINE test #-}




type MParser = Megaparsec.Parsec Void Text

megaparsec_a :: MParser Bool
megaparsec_a = Megaparsec.satisfy ('a' ==) >> megaparsec_a


main = do
    hSetBuffering stdout NoBuffering


    let s = "100000000" :: String
        x = read s :: Int

    txt  <- eval $ Text.replicate x "a"
    str  <- eval $ replicate x 'a'
    slst <- eval $ sreplicate x 'a'
    putStrLn "\nrunning"
    putStr "test " >> timeIt (eval (test txt x))

    timeIt (eval $ Megaparsec.parse megaparsec_a "" txt)


data SList = SCons !Char !SList
           | SNull
           deriving (Show, Generic)

sreplicate :: Int -> Char -> SList
sreplicate 0 !c = SNull
sreplicate i !c = SCons c $ sreplicate (i - 1) c

instance NFData (SList)

type instance Item SList = Char
instance IsStream SList where
    uncons = \case
        SCons c l -> Just (c,l)
        SNull     -> Nothing
    {-# INLINE uncons #-}


-- data T a b = T {-# UNPACK #-}!a {-# UNPACK #-}!b deriving (Show, Generic)
-- instance (NFData a, NFData b) => NFData (T a b)
--
-- data SMaybe a = SJust {-# UNPACK #-} !a
--               | SNothing
--               deriving (Show, Generic)
-- instance NFData a => NFData (SMaybe a)
