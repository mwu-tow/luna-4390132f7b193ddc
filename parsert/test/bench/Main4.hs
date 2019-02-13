{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}

module Main where

import Prelude hiding (Either, Left, Right)
import qualified Prelude as P
import Control.Monad.Trans
import Control.DeepSeq (NFData, force)

import Data.Monoid
import Criterion.Main
import Control.Monad.State.Layered
import qualified Data.Text as Text
import           Data.Text (Text)
import Control.Monad.Identity
import GHC.Generics (Generic)
import GHC.IO          as X (evaluate)
import Data.String (fromString)
import Control.Lens
import Data.Void

import Unsafe.Coerce (unsafeCoerce)

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


--
-- | WARNING: -XStrict enabled in this file
--


data Result e a = Error         e
                | NonProgResult a
                | ProgResult    a
                deriving (Eq, Generic, Ord, Read, Show, Functor, Foldable, Traversable)

newtype ResultT e m a = ResultT { runResultT :: m (Result e a) }

instance Monad m => Functor (ResultT e m) where
    fmap f = \ a -> ResultT $ liftM (fmap f) $ runResultT a ; {-# INLINE fmap #-}

instance Monad m => Applicative (ResultT e m) where
    pure a = ResultT $ return $ NonProgResult a ; {-# INLINE pure #-}
    (<*>) = undefined

instance Monad m => Monad (ResultT e m) where
    return  = pure ; {-# INLINE return #-}
    m >>= f = ResultT $ runResultT m >>= \case
        Error         e -> return $ Error e
        NonProgResult a -> runResultT $ f a
        ProgResult    a -> (runResultT $ f a) >>= return . \case
            NonProgResult a' -> ProgResult a'
            s                -> s

instance (NFData e, NFData a) => NFData (Result e a)

class Monad m => ProgressMonad m where
    returnProgressed :: forall a. a -> m a

instance {-# OVERLAPPABLE #-} (ProgressMonad m, Monad (t m), MonadTrans t) => ProgressMonad (t m) where
    returnProgressed = lift . returnProgressed ; {-# INLINE returnProgressed #-}

instance Monad m => ProgressMonad (ResultT e m) where
    returnProgressed a =  ResultT $ return $ ProgResult a ; {-# INLINE returnProgressed #-}


------------------------
-- === Primitives === --
------------------------

-- === Either === --

data    Either  e   a = Left !e | Right !a deriving (Eq, Generic, Ord, Read, Show, Functor)
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (EitherT e m) where
  fmap f = \ a -> EitherT $ liftM (fmap f) $ runEitherT a ; {-# INLINE fmap #-}

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
  fail a = EitherT $ fail a
  {-# INLINE fail #-}


-- === Bool === --

data XBool = XTrue | XFalse deriving (Show, Generic)

(|||) :: XBool -> XBool -> XBool
(|||) a b = case a of
    XTrue  -> a
    XFalse -> b
{-# INLINE (|||) #-}


-- === Tuple === --

data T a b = T a b deriving (Generic, Show, Functor)


------------------------
-- === FailParser === --
------------------------

-- === Definition === --

newtype FailParser m a = FailParser { fromFailParser :: EitherT () m (T XBool a) } deriving (Functor)

instance Monad m => Applicative (FailParser m) where
    pure  = undefined
    (<*>) = undefined

instance Monad m => Monad (FailParser m) where
    return a = FailParser $ return $ (T XFalse a) ; {-# INLINE return #-}
    FailParser ma >>= (f) = FailParser $ do
        T b  a  <- ma
        T b' a' <- fromFailParser $ f a
        return $ T b' a'
    {-# INLINE (>>=) #-}
    _ >> _ = undefined ; {-# INLINE (>>) #-}


instance MonadTrans FailParser where
    lift m = FailParser $ EitherT $ fmap (Right . T XFalse) m

-- === Running === --

failParser :: m (Either () (T XBool a)) -> FailParser m a
failParser a = FailParser $ EitherT a ; {-# INLINE failParser #-}

runFailParser :: forall m a. FailParser m a -> m (Either () (T XBool a))
runFailParser f = runEitherT $ fromFailParser f ; {-# INLINE runFailParser #-}


-- === MonadFailedParser === --

class Monad m => MonadFailedParser m where
    failed :: m a

instance {-# OVERLAPPABLE #-} (MonadFailedParser m, MonadTrans t, Monad (t m))
      => MonadFailedParser (t m) where
    failed = lift failed ; {-# INLINE failed #-}

instance Monad m => MonadFailedParser (FailParser m) where
    failed = failParser $ return $ Left () ; {-# INLINE failed #-}

instance Monad m => MonadFailedParser (ResultT () m) where
    failed = ResultT $ return $ Error ()


-- === ProgressMonad === --

instance Monad m => ProgressMonad (FailParser m) where
    returnProgressed a = failParser $ return $ Right $ T XTrue a ; {-# INLINE returnProgressed #-}


------------------------
-- === FailParser === --
------------------------

-- === Definition === --

newtype FailParser2 m a = FailParser2 { fromFailParser2 :: EitherT () m a } deriving (Functor, Applicative, Monad)

-- === Running === --

failParser2 :: m (Either () a) -> FailParser2 m a
failParser2 a = FailParser2 $ EitherT a ; {-# INLINE failParser2 #-}

runFailParser2 :: forall m a. FailParser2 m a -> m (Either () a)
runFailParser2 f = runEitherT $ fromFailParser2 f ; {-# INLINE runFailParser2 #-}


-- === MonadFailedParser === --

instance Monad m => MonadFailedParser (FailParser2 m) where
    failed = failParser2 $ return $ Left () ; {-# INLINE failed #-}




-----------------------
-- === Main loop === --
-----------------------

-- parserLoop_manual :: StateT Text (FailParser Identity) Bool
-- parserLoop_manual = parserStep >> parserLoop

parserLoop_manual :: Int -> Text -> (Bool, Int)
parserLoop_manual !i !t = case Text.uncons t of
    Just (!t, !ts) -> if t == 'a' then parserLoop_manual (i + 1) ts else (False,i)
    Nothing        -> (True,i)
-- {-# INLINE parserStep #-}


parserLoop :: StateT Text (FailParser Identity) Bool
parserLoop = parserStep >> parserLoop

parserStep :: StateT Text (FailParser Identity) Char
parserStep = get @Text >>= \s -> case Text.uncons s of
    Just (t, s') -> if t == 'a' then put @Text s' >> return t else failed
    Nothing      -> failed
{-# INLINE parserStep #-}


parserLoop2 :: StateT Text (ResultT () Identity) Bool
parserLoop2 = parserStep2 >> parserLoop2

parserStep2 :: StateT Text (ResultT () Identity) Char
parserStep2 = get @Text >>= \s -> case Text.uncons s of
    Just (t, s') -> if t == 'a' then put @Text s' >> returnProgressed t else failed
    Nothing      -> failed
{-# INLINE parserStep2 #-}


parserLoop4 :: StateT Int ((StateT Text (FailParser2 Identity))) Bool
-- parserLoop4 :: StateT Text (StateT Bool (FailParser2 (State Int))) Bool
parserLoop4 = parserStep4 >> parserLoop4

parserStep4 :: StateT Int ((StateT Text (FailParser2 Identity))) Char
-- parserStep4 :: StateT Text (StateT Bool (FailParser2 (State Int))) Char
parserStep4 = get @Text >>= \s -> case Text.uncons s of
    Just (t, s') -> if t == 'a' then put @Text s' >> (do {s <- get @Int; put @Int (s+1)}) >> return t else failed
    Nothing      -> failed
{-# INLINE parserStep4 #-}


-- {-# SPECIALIZE parserLoop4 :: StateT Text (StateT Bool (FailParser Identity)) Bool #-}
-- {-# SPECIALIZE parserStep4 :: StateT Text (StateT Bool (FailParser Identity)) Char #-}


-- === Criterion === --

instance NFData XBool
instance (NFData l, NFData r) => NFData (Either l r)
instance (NFData a, NFData b) => NFData (T a b)

genText :: Int -> Text
genText i = fromString $ replicate i 'a' ; {-# INLINE genText #-}







type family Token s

data ParseError t e = ParseError deriving (Show, Generic)
instance NFData (ParseError t e)

instance Monoid (ParseError t e) where
    mempty = ParseError
    mappend = const

data S s = S
  { stateInput :: {-# UNPACK #-} !s
  , stateTokensProcessed :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Generic)
instance NFData s => NFData (S s)

newtype Hints t = Hints [Int] deriving (Monoid)
-- newtype Hints t = Hints [Set (ErrorItem t)] deriving (Semigroup, Monoid)


newtype ParserT e s m a = ParserT { unParser :: forall b. P e s m a b -> m b }

data P e s m a b
   = P { st   :: !(S s)
       , cok  :: !(a -> S s -> m b) -- consumed-OK
       , cerr :: !(ParseError (Token s) e -> S s -> m b) -- consumed-error
       , eok  :: !(a -> S s -> m b) -- empty-OK
       , eerr :: !(ParseError (Token s) e -> S s -> m b) -- empty-error
       }
makeLenses ''P

retarget :: P e s m a b -> P e s m a b'
retarget = unsafeCoerce ; {-# INLINE retarget #-}

data Reply e s a = Reply (S s) Consumption (R (Token s) e a) deriving (Generic)
instance (NFData s, NFData a) => NFData (Reply e s a)

data R t e a
  = OK a                   -- ^ Parser succeeded
  | Error2 (ParseError t e) -- ^ Parser failed
  deriving (Generic)
instance NFData a => NFData (R t e a)

data Consumption
  = Consumed -- ^ Some part of input stream was consumed
  | Virgin   -- ^ No input was consumed
  deriving (Generic)
instance NFData Consumption

instance Functor (ParserT e s m) where
    fmap f pr = ParserT $ \p -> unParser pr $ p {cok = cok p . f, eok = eok p . f} ; {-# INLINE fmap #-}

-- instance Applicative (ParserT e s m)
instance Monad (ParserT e s m) where
  pr >>= f = ParserT $ \p ->
    let mcok a s' = unParser (f a) $ p {st = s', eok = cok p, eerr = cerr p}
        meok a s' = unParser (f a) $ p {st = s'}
    in unParser pr $ p { cok = mcok, eok = meok }
  {-# INLINE (>>=) #-}


instance Applicative (ParserT e s m) where
  pure x = ParserT $ \p -> eok p x (st p) ; {-# INLINE pure #-}
  (<*>)    = pAp
  -- p1 *> p2 = p1 `pBind` const p2
  -- p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }
  --


pAp :: ParserT e s m (a -> b) -> ParserT e s m a -> ParserT e s m b
pAp m k = ParserT $ \p ->
  let mcok a s' = unParser k $ p {st = s', cok = cok p . a, eok = cok p . a, eerr = cerr p}
      meok a s' = unParser k $ p {st = s', cok = cok p . a, eok = eok p . a}
  in unParser m $ p {cok = mcok, eok = meok}
{-# INLINE pAp #-}


runParserT :: Monad m
  => ParserT e s m a -- ^ Parser to run
  -> S s       -- ^ Initial state
  -> m (Reply e s a)
runParserT p s = unParser p (P s cok cerr eok eerr)
  where cok a s'    = return $ Reply s' Consumed (OK a)
        cerr err s' = return $ Reply s' Consumed (Error2 err)
        eok a s'    = return $ Reply s' Virgin   (OK a)
        eerr err s' = return $ Reply s' Virgin   (Error2 err)



pPlus :: Ord e
  => ParserT e s m a
  -> ParserT e s m a
  -> ParserT e s m a
pPlus m n = ParserT $ \(P s cok cerr eok eerr) ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s'     = eok x s'
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
        in unParser n (P s cok ncerr neok neerr)
  in unParser m (P s cok cerr eok meerr)
{-# INLINE pPlus #-}

-- | From two states, return the one with the greater number of processed
-- tokens. If the numbers of processed tokens are equal, prefer the second
-- state.

longestMatch :: S s -> S s -> S s
longestMatch _ s = undefined -- s -- s1@(State _ _ tp1 _) s2@(State _ _ tp2 _) =
  -- case tp1 `compare` tp2 of
  --   LT -> s2
  --   EQ -> s2
  --   GT -> s1

toHints :: ParseError t e -> Hints t
toHints _ = Hints []



-- | @accHints hs c@ results in “OK” continuation that will add given hints
-- @hs@ to third argument of original continuation @c@.

accHints
  :: Hints t           -- ^ 'Hints' to add
  -> (a -> S s -> Hints t -> m b) -- ^ An “OK” continuation to alter
  -> (a -> S s -> Hints t -> m b)
accHints _ a = a ; {-# INLINE accHints #-}
-- accHints hs1 c x s hs2 = c x s (hs1 <> hs2) ; {-# INLINE accHints #-}



-- | @withHints hs c@ makes “error” continuation @c@ use given hints @hs@.
--
-- Note that if resulting continuation gets 'ParseError' that has custom
-- data in it, hints are ignored.

withHints :: Hints (Token s)   -- ^ Hints to use
  -> (ParseError (Token s) e -> S s -> m b) -- ^ Continuation to influence
  -> (ParseError (Token s) e -> S s -> m b)
withHints _ c = c
  -- case e of
  --   TrivialError pos us ps -> c (TrivialError pos us (E.unions (ps : ps')))
  --   _ -> c e
{-# INLINE withHints #-}


class Stream s where
    take1_ :: s -> Maybe (Token s, s)

type instance Token Text = Char
instance Stream Text where
    take1_ = Text.uncons

--
pToken :: forall e s m a. Stream s
  => (Token s -> Maybe a)
  -> Maybe (Token s)
  -> ParserT e s m a
pToken test mtoken = ParserT $ \ (P s@(S input tp) cok _ _ eerr) -> case take1_ input of
    Nothing     -> eerr ParseError s
    Just (c,cs) -> case test c of
        Nothing -> eerr ParseError s
        Just x  -> cok x newstate where
          newstate = S cs (tp + 1)
{-# INLINE pToken #-}


pToken2 :: (MonadState Text m, MonadState Int m, PCls m) => (Char -> Maybe a) -> m a
pToken2 test = do
    s <- get @Text
    case take1_ s of
        Nothing -> pfail
        Just (c,cs) -> case test c of
            Nothing -> pfail
            Just x -> put @Text cs >> pok x

class PCls m where
    pfail :: forall a. m a
    pok   :: forall a. a -> m a

instance (PCls m, Monad m) => PCls (StateT s m) where
    pfail = lift pfail
    pok   = lift . pok

instance PCls (ParserT e Text m) where
    pfail = ParserT $ \ (P s cok _ _ eerr) -> eerr ParseError s
    pok  a = ParserT $ \ (P s@(S input tp) cok _ _ eerr) -> cok a s

--
--     ParserT $ \ (P s@(S input tp) cok _ _ eerr) -> case take1_ input of
--     Nothing     -> eerr ParseError s
--     Just (c,cs) -> case test c of
--         Nothing -> eerr ParseError s
--         Just x  -> cok x newstate where
--           newstate = S cs (tp + 1)
-- {-# INLINE pToken #-}

satisfy :: Stream s => (Token s -> Bool) -> ParserT e s m (Token s)
satisfy !f = pToken (\ !t -> if f t then Just t else Nothing) Nothing

satisfy2 :: (MonadState Text m, MonadState Int m, PCls m) => (Char -> Bool) -> m Char
satisfy2 !f = pToken2 (\ !t -> if f t then Just t else Nothing)


-- parserLoop3 :: StateT Text (ParserT e Text m) Bool
-- parserLoop3 = lift (satisfy ('a' ==)) >> parserLoop3


parserLoop3 :: StateT Int (StateT Text (ParserT e Text m)) Bool
parserLoop3 = satisfy2 ('a' ==) >> parserLoop3

parserStep3 :: StateT Text (ResultT () Identity) Char
parserStep3 = get @Text >>= \s -> case Text.uncons s of
    Just (t, s') -> if t == 'a' then put @Text s' >> returnProgressed t else failed
    Nothing      -> failed
{-# INLINE parserStep3 #-}



countLoop_manual :: Int -> Int -> Int
countLoop_manual t i = case i of
    0 -> t
    _ -> countLoop_manual (t + 1) (i - 1)


countLoop_state :: Int -> Int -> Int
countLoop_state t i = evalState @Int (countLoop_state' i) t ; {-# INLINE countLoop_state #-}

countLoop_state' :: Int -> State Int Int
countLoop_state' = \case
    0 -> get @Int
    i -> do
        t <- get @Int
        put @Int (t + 1)
        countLoop_state' (i - 1)
{-# INLINABLE countLoop_state' #-}

{-# SPECIALIZE countLoop_state' :: Int -> State Int Int #-}



repeatM :: Monad m => Int -> m a -> m ()
repeatM i f = let r 0 = pure (); r i = f >> r (i - 1) in r i ; {-# INLINE repeatM #-}


incState2 :: (MonadState Int m, MonadState Text m) => m ()
incState2 = modify_ @Int succ >> modify_ @Text id ; {-# INLINE incState2 #-}



type MParser = Megaparsec.Parsec Void Text

megaparsec_a :: MParser Bool
megaparsec_a = Megaparsec.satisfy ('a' ==) >> megaparsec_a

a_parsing_main :: IO ()
a_parsing_main = do
    defaultMain
        [ bench "0  trans (10e6)"     $ nf (\ !i -> runIdentity $ runFailParser $ flip (evalStateT @Int) 0 $ flip (evalStateT @Text) "" $ repeatM i incState2) 1000000
        -- [ env (return $ 10^7) $ bench "pure count loop"  . nf (countLoop_manual 0)
        -- , env (return $ 10^7) $ bench "state count loop" . nf (countLoop_state  0)
        -- -- , env (return $ 10^7) $ bench "state count loop" . nf (parserLoop_manual 0)
        -- , env (return $ genText $ 10^7) $ bench "pure loop" . nf (parserLoop_manual 0)
        -- , env (return $ genText $ 10^7) $ bench "a*" . nf (runIdentity . runFailParser . evalStateT parserLoop)
        -- -- , env (return $ genText $ 10^7) $ bench "a*" . nf (runIdentity . runResultT    . evalStateT parserLoop2)
        -- , env (return $ genText $ 10^7) $ bench "a*" . nf (\t -> runIdentity . runFailParser2 . flip evalStateT t $ flip (evalStateT @Int) 0 parserLoop4)
        -- , env (return $ genText $ 10^7) $ bench "a*" . nf (runIdentity . flip (evalStateT @Int) 0 . runFailParser . flip (evalStateT @Bool) False . evalStateT parserLoop4)
        , env (return $ genText $ 10^7) $ bench "a*" . nf (\t -> runIdentity . flip runParserT (S t 0) . flip (evalStateT @Text) t . flip (evalStateT @Int) 0 $ parserLoop3)
        , env (return $ genText $ 10^7) $ bench "a*" . nf (Megaparsec.parse megaparsec_a "")
        ]



main = a_parsing_main

-- newtype ParserT e s m a = ParserT
--   { unParser :: forall b.
--      S s
--   -> (a -> S s   -> Hints (Token s) -> m b) -- consumed-OK
--   -> (ParseError (Token s) e -> S s -> m b) -- consumed-error
--   -> (a -> S s   -> Hints (Token s) -> m b) -- empty-OK
--   -> (ParseError (Token s) e -> S s -> m b) -- empty-error
--   -> m b
--   }
