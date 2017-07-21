{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
-- {-# LANGUAGE Strict                 #-}
{-# EXT      InlineAll              #-}

module Text.Parsert where

import qualified Prelude as P
import qualified Data.List as List
import Prologue hiding (catch, drop, (.))
import Type.Inference

import Control.Monad.State.Layered
import GHC.Exts (Any)
import Control.Monad.Branch

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x) ; {-# INLINE (.) #-}

---------------------------
-- === Error parsing === --
---------------------------


-- === Definition === --

type family Error (m :: * -> *) :: *

class Monad m => MonadThrowParser m where
    throw     :: forall a.           Error m  -> m a
    throwMany :: forall a .NonEmpty (Error m) -> m a
    throw = throwMany . pure

class Monad m => MonadErrorBuilder t m e where
    buildError :: t -> m e

type MonadErrorParser t m = (MonadThrowParser m, MonadErrorBuilder t m (Error m))


-- === Utils === --

raise     :: MonadErrorParser t m =>          t -> m a
raiseMany :: MonadErrorParser t m => NonEmpty t -> m a
raise     = throw     <=< buildError      ; {-# INLINE raise     #-}
raiseMany = throwMany <=< mapM buildError ; {-# INLINE raiseMany #-}

-- === Primitive errors === --

newtype UncaughtError = UncaughtError String deriving (Show)
instance (Monad m, Show t) => MonadErrorBuilder t m UncaughtError where
    buildError = return . UncaughtError . show ; {-# INLINE buildError #-}

newtype DescibedError = DescibedError Text deriving (Show)

descibedError  :: MonadErrorParser DescibedError m => Text -> m a
descibedError' :: (MonadErrorParser DescibedError m, Convertible t Text) => t -> m a
descibedError  = raise . DescibedError   ; {-# INLINE descibedError  #-}
descibedError' = descibedError . convert ; {-# INLINE descibedError' #-}


-- === Instances === --

type instance Error (StateT s m) = Error m
instance MonadThrowParser m => MonadThrowParser (StateT s m) where
    throw     = lift . throw     ; {-# INLINE throw     #-}
    throwMany = lift . throwMany ; {-# INLINE throwMany #-}



------------------------------
-- === Progress parsing === --
------------------------------

-- === Definition === --

newtype Progress = Progress Bool deriving (Show)
makeLenses ''Progress

class Monad m => MonadProgressParser m where
    putProgress :: Bool -> m ()
    getProgress :: m Bool
    try         :: m a -> m a

    default putProgress :: (m ~ t n, MonadTrans t, MonadProgressParser n) => Bool -> m ()
    default getProgress :: (m ~ t n, MonadTrans t, MonadProgressParser n) => m Bool
    putProgress = lift . putProgress ; {-# INLINE putProgress #-}
    getProgress = lift   getProgress ; {-# INLINE getProgress #-}

class Monad m => MonadCatchParser m where
    catch :: m a -> m (Either (NonEmpty (Error m)) a)


-- === Utils === --

recover :: MonadCatchParser m => (NonEmpty (Error m) -> m a) -> m a -> m a
recover f m = catch m >>= \case
    Right a -> return a
    Left  e -> f e
{-# INLINE recover #-}

recover_ :: MonadCatchParser m => (NonEmpty (Error m) -> m a) -> m b -> m ()
recover_ f m = recover (void . f) (void m) ; {-# INLINE recover_ #-}

replaceErrorsWith :: (MonadCatchParser m, MonadErrorParser e m) => e -> m a -> m a
replaceErrorsWith e m = catch m >>= \case
    Right a -> return a
    Left  _ -> raise e
{-# INLINE replaceErrorsWith #-}

setProgress   :: MonadProgressParser m => m ()
unsetProgress :: MonadProgressParser m => m ()
setProgress   = putProgress True  ; {-# INLINE setProgress   #-}
unsetProgress = putProgress False ; {-# INLINE unsetProgress #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} MonadProgressParser m
 => MonadProgressParser (StateT s m) where
    try = mapStateT try ; {-# INLINE try #-}

instance {-# OVERLAPPABLE #-} MonadCatchParser m
 => MonadCatchParser (StateT s m) where
     catch ma = stateT $ \s -> catch (runStateT ma s)
               <&> \case Left  e      -> (Left  e, s)
                         Right (a, t) -> (Right a, t)
     {-# INLINE catch #-}


---------------------------
-- === Token parsing === --
---------------------------

-- === Definition === --

type family Token (m :: * -> *) :: *

class Monad m => MonadTokenParser m where
    getNextToken  :: m (Maybe (Token m))
    viewNextToken :: m (Maybe (Token m))
    default getNextToken  :: (m ~ t m', Token m' ~ Token (t m'), MonadTokenParser m', MonadTrans t, Monad m') => m (Maybe (Token m))
    default viewNextToken :: (m ~ t m', Token m' ~ Token (t m'), MonadTokenParser m', MonadTrans t, Monad m') => m (Maybe (Token m))
    getNextToken  = lift getNextToken  ; {-# INLINE getNextToken  #-}
    viewNextToken = lift viewNextToken ; {-# INLINE viewNextToken #-}

class MonadTokenParser m => MonadFiniteTokenParser m where
    viewNextTokens :: m [Token m]
    default viewNextTokens :: (m ~ t m', Token m' ~ Token (t m'), MonadFiniteTokenParser m', MonadTrans t, Monad m') => m [Token m]
    viewNextTokens = lift viewNextTokens ; {-# INLINE viewNextTokens #-}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Token m ~ Token (t m), MonadTokenParser m)       => MonadTokenParser       (t m)
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Token m ~ Token (t m), MonadFiniteTokenParser m) => MonadFiniteTokenParser (t m)


-- === Utils === --

takeToken :: (MonadTokenParser m, MonadErrorParser EmptyStreamError m) => m (Token m)
takeToken = maybe (raise EmptyStreamError) return =<< getNextToken ; {-# INLINE takeToken #-}

anyToken :: (MonadTokenParser m, MonadErrorParser EmptyStreamError m, MonadProgressParser m) => m (Token m)
anyToken = takeToken <* setProgress ; {-# INLINE anyToken #-}

dropToken :: (MonadTokenParser m, MonadErrorParser EmptyStreamError m, MonadProgressParser m) => m ()
dropToken = void anyToken ; {-# INLINE dropToken #-}

token :: (MonadTokenParser m, MonadProgressParser m, Alternative m, Eq (Token m), MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m) => Token m -> m (Token m)
token = satisfy . (==) ; {-# INLINE token #-}

notToken :: (MonadTokenParser m, MonadProgressParser m, Alternative m, Eq (Token m), MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m) => Token m -> m (Token m)
notToken = satisfy . (/=) ; {-# INLINE notToken #-}

tokens :: (MonadTokenParser m, MonadProgressParser m, Alternative m, Eq (Token m), MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m) => [Token m] -> m [Token m]
tokens = try . mapM token ; {-# INLINE tokens #-}

satisfy, satisfyNot :: (MonadTokenParser m, MonadProgressParser m, Alternative m, MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m) => (Token m -> Bool) -> m (Token m)
satisfyNot = satisfy . fmap not ; {-# INLINE satisfyNot #-}
satisfy f  = takeToken >>= \tok -> if f tok
    then tok <$ setProgress
    else raise SatisfyError
{-# INLINE satisfy #-}

notFollowedBy :: (MonadTokenParser m, MonadProgressParser m, Alternative m, MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m, MonadCatchParser m) => m a -> m ()
notFollowedBy m = catch m >>= \case
    Right _ -> raise SatisfyError
    Left  _ -> return ()
{-# INLINE notFollowedBy #-}

data SatisfyError     = SatisfyError     deriving (Show)
data EmptyStreamError = EmptyStreamError deriving (Show)
data PreviewError     = PreviewError Any


---------------------------
-- === Offset parser === --
---------------------------

-- === Definition === --

newtype Offset = Offset Int deriving (Show, Num, Enum, Ord, Eq)
makeLenses ''Offset


-- === Running === --

evalOffsetRegister :: Monad m => StateT Offset m a -> m a
evalOffsetRegister = flip evalStateT (def :: Offset) ; {-# INLINE evalOffsetRegister #-}


-- === Instances === --

instance Convertible Int    Offset where convert = wrap   ; {-# INLINE convert #-}
instance Convertible Offset Int    where convert = unwrap ; {-# INLINE convert #-}

instance Default   Offset where def    = mempty   ; {-# INLINE def    #-}
instance Mempty    Offset where mempty = Offset 0 ; {-# INLINE mempty #-}
instance Semigroup Offset where (<>)   = (+)      ; {-# INLINE (<>)   #-}

type instance Token (StateT Offset m) = Token m
instance MonadTokenParser m => MonadTokenParser (StateT Offset m) where
    getNextToken = modify_ @Offset succ >> lift getNextToken ; {-# INLINE getNextToken #-}



---------------------------
-- === Stream parser === --
---------------------------

-- === Definition === --

data Stream s = Stream { _streamSplitter :: s -> Maybe (Item s, s)
                       , _streamBuffer   :: s
                       }
makeLenses ''Stream


-- === Utils === --

evalStreamProvider :: Monad m => Stream s -> StateT (Stream s) m a -> m a
evalStreamProvider = flip evalStateT ; {-# INLINE evalStreamProvider #-}

splitStream :: Stream s -> Maybe (Item s, Stream s)
splitStream (Stream splitter buffer) = Stream splitter <<$>> splitter buffer ; {-# splitStream #-}


-- === Example streams === --

listStream :: [a] -> Stream [a]
listStream = Stream luncons ; {-# INLINE listStream #-}

luncons        :: [a] -> Maybe (a, [a])
luncons []     = Nothing
luncons (x:xs) = Just (x, xs)
{-# INLINE luncons #-}

-- === Instances === --

type instance Token (StateT (Stream s) m) = Item s
instance Monad m => MonadTokenParser (StateT (Stream s) m) where
    viewNextToken = fmap fst . splitStream <$> get @Stream ; {-# INLINE viewNextToken #-}
    getNextToken = modify @Stream go where
        go s = case splitStream s of
            Nothing     -> (Nothing, s)
            Just (t,s') -> (Just t, s')
        {-# INLINE go #-}
    {-# INLINE getNextToken #-}

instance (Monad m, Convertible' s [Item s]) => MonadFiniteTokenParser (StateT (Stream s) m) where
    viewNextTokens = convert' . view streamBuffer <$> get @Stream ; {-# INLINE viewNextTokens #-}


------------------------
-- === FailParser === --
------------------------

-- === Definition === --

newtype FailParser e m a = FailParser (EitherT (NonEmpty e) m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
makeLenses ''FailParser


-- === Running === --

failParser :: m (Either (NonEmpty e) a) -> FailParser e m a
failParser = wrap . EitherT ; {-# INLINE failParser #-}

runFailParser :: forall e m a. FailParser e m a -> m (Either (NonEmpty e) a)
runFailParser = runEitherT . unwrap ; {-# INLINE runFailParser #-}


-- === Instances === --

type instance Token (FailParser e m) = Token m
instance MonadProgressParser m => MonadPlus   (FailParser e m)
instance MonadProgressParser m => Alternative (FailParser e m) where
    empty = error "Do not use empty, use `raise` instead!" ; {-# INLINE empty #-} -- throw "Unknown error"
    l <|> r = failParser $ do
        p <- getProgress
        unsetProgress
        la <- runFailParser l
        lp <- getProgress
        putProgress p
        if lp then return la else case la of
            Right _ -> return la
            Left  e -> do
                ra <- runFailParser r
                rp <- getProgress
                return $ if rp then ra else mapLeft (e <>) ra
    {-# INLINE (<|>) #-}


type instance Error (FailParser e m) = e
instance MonadProgressParser m => MonadProgressParser (FailParser e m) where
    try m = failParser $ do
        a <- runFailParser m
        when (isLeft a) unsetProgress
        return a
    {-# INLINE try #-}

instance Monad m => MonadCatchParser (FailParser e m) where catch     = failParser . fmap Right . runFailParser ; {-# INLINE catch     #-}
instance Monad m => MonadThrowParser (FailParser e m) where throwMany = failParser . pure . Left                ; {-# INLINE throwMany #-}

-- FIXME: We should consider lifting MonadBranch here, but we need to add Parsert debugging toolset better than IO prints
instance MonadBranch m => MonadBranch (FailParser e m) where
    branched = wrapped %~ mapEitherT branched ; {-# INLINE branched #-}


-------------------------
-- === BackTracker === --
-------------------------

-- === Definition === --

newtype Backtracker = Backtracker { _progress :: Bool } deriving (Show)
makeLenses ''Backtracker

instance Monad m => MonadProgressParser (StateT Backtracker m) where
    getProgress = unwrap <$> get @Backtracker ; {-# INLINE getProgress #-}
    putProgress = put @Backtracker . wrap     ; {-# INLINE putProgress #-}
    try         = id                          ; {-# INLINE try         #-}


-- === Running === --

evalBacktracker :: Monad m => StateT Backtracker m a -> m a
evalBacktracker = flip evalStateT (def :: Backtracker) ; {-# INLINE evalBacktracker #-}


-- === Instances === --

type instance Token (StateT Backtracker m) = Token m
instance Default Backtracker where def = Backtracker False


---------------------------
-- === BranchBreaker === --
---------------------------

-- === Definition === --

-- | FIXME: BranchBreaker is only used as a layer between Parsert transformers and IO when used MonadBranch.
--          We could remove it as soon as we introduce Parsert debugging / logging which could work with MonadBranch.

newtype BranchBreaker m a = BranchBreaker (IdentityT m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
makeLenses ''BranchBreaker


-- === Utils === --

runBranchBreaker :: BranchBreaker m a -> m a
runBranchBreaker = runIdentityT . unwrap ; {-# INLINE runBranchBreaker #-}


-- === Instances === --

instance Monad m => MonadBranch (BranchBreaker m) where branched = id ; {-# INLINE branched #-}



----------------------------
-- === IO parser base === --
----------------------------

type instance Error IO = UncaughtError

instance MonadProgressParser IO where
    putProgress = const $ return () ; {-# INLINE putProgress #-}
    getProgress = return False      ; {-# INLINE getProgress #-}
    try         = id                ; {-# INLINE try         #-}

instance MonadThrowParser IO where
    throwMany = fail . show ; {-# INLINE throwMany #-}
    throw     = fail . show ; {-# INLINE throw     #-}



----------------------------------
-- === Identity parser base === --
----------------------------------

type instance Error Identity = UncaughtError

instance MonadProgressParser Identity where
    putProgress = const $ return () ; {-# INLINE putProgress #-}
    getProgress = return False      ; {-# INLINE getProgress #-}
    try         = id                ; {-# INLINE try         #-}

instance MonadThrowParser Identity where
    throwMany = error . show ; {-# INLINE throwMany #-}
    throw     = error . show ; {-# INLINE throw     #-}



------------------------------
-- === Results register === --
------------------------------

-- === Definition === --

type ResultRegister r = StateT (Results r)
newtype Results a = Results [a] deriving (Show, Functor, Foldable, Traversable, Default, Semigroup, Mempty, P.Monoid)
makeLenses ''Results

type family Result (m :: * -> *) :: * where
    Result (StateT (Results a) _) = a
    Result (t m) = Result m


-- === Utils === --

data ResultSource
type MonadResultParser  t m = (MonadResultRegister m, MonadResultBuilder t m (Result m), t ~ Infer ResultSource m)
type MonadResultParser' t m = (MonadResultRegister m, MonadResultBuilder t m (Result m), TryInfer ResultSource m t)

addResult  :: MonadResultParser  t m => t -> m ()
addResult' :: MonadResultParser' t m => t -> m ()
addResult  = buildResult >=> registerResult ; {-# INLINE addResult  #-}
addResult' = buildResult >=> registerResult ; {-# INLINE addResult' #-}

runResultRegister  :: forall res m a . Monad m => ResultRegister res m a -> m (a, [res])
evalResultRegister :: forall res m a . Monad m => ResultRegister res m a -> m a
execResultRegister :: forall res m a . Monad m => ResultRegister res m a -> m [res]
runResultRegister  = fmap3 (reverse . unwrap) runDefStateT ; {-# INLINE runResultRegister  #-}
evalResultRegister = fmap2 fst runResultRegister           ; {-# INLINE evalResultRegister #-}
execResultRegister = fmap2 snd runResultRegister           ; {-# INLINE execResultRegister #-}


-- === ResultRegister === --

class Monad m => MonadResultRegister m where
    registerResult :: Result m -> m ()

instance {-# OVERLAPPABLE #-} (MonadResultRegister m, Result (t m) ~ Result m, Monad (t m), MonadTrans t)
 => MonadResultRegister (t m) where
    registerResult = lift . registerResult ; {-# INLINE registerResult #-}

instance Monad m => MonadResultRegister (StateT (Results a) m) where
    registerResult res = modify_ @Results $ wrapped %~ (res:) ; {-# INLINE registerResult #-}


-- === ResultBuilder === --

class Monad m => MonadResultBuilder t m a where
    buildResult :: t -> m a

instance {-# OVERLAPPABLE #-} Monad m => MonadResultBuilder t m t where
    buildResult = return ; {-# INLINE buildResult #-}


-- === Instances === --

type instance Token (StateT (Results r) m) = Token m



------------------------------
-- === History register === --
------------------------------

-- === Definition === --

type HistoryRegister tok = StateT (History tok)
newtype History tok = History [tok] deriving (Functor, Foldable, Traversable, Semigroup, Mempty, P.Monoid, Default)
makeLenses ''History


-- === Running === --

runHistoryRegister  :: (tok ~ Token m, Monad m) => HistoryRegister tok m a -> m (a, History tok)
evalHistoryRegister :: (tok ~ Token m, Monad m) => HistoryRegister tok m a -> m a
execHistoryRegister :: (tok ~ Token m, Monad m) => HistoryRegister tok m a -> m (History tok)
runHistoryRegister  = runDefStateT  ; {-# INLINE runHistoryRegister  #-}
evalHistoryRegister = evalDefStateT ; {-# INLINE evalHistoryRegister #-}
execHistoryRegister = execDefStateT ; {-# INLINE execHistoryRegister #-}


-- === Instances === --

type instance Token (StateT (History tok) m) = Token m
instance (MonadTokenParser m, tok ~ Token m) => MonadTokenParser (StateT (History tok) m) where
    getNextToken = withJustM (lift getNextToken) $ \tok -> Just tok <$ modify_ @History (wrapped %~ (tok:)) ; {-# INLINE getNextToken #-}

instance (Show tok) => Show (History tok) where
       showsPrec d (History toks) = showParen' d $
            showString "History " . showsPrec' (reverse toks)
       {-# INLINE showsPrec #-}




----------------------------------
-- === Standard combinators === --
----------------------------------

choice :: (Alternative m, MonadErrorParser SatisfyError m) => [m a] -> m a
choice = foldr (<|>) $ raise SatisfyError ; {-# INLINE choice #-}

option' :: Alternative m => a -> m a -> m a
option :: (Alternative m, MonadProgressParser m) => a -> m a -> m a
option' a p = p <|> pure a      ; {-# INLINE option' #-}
option  a p = option' a $ try p ; {-# INLINE option  #-}

option'_ :: Alternative m                          => m a -> m ()
option_  :: (Alternative m, MonadProgressParser m) => m a -> m ()
option'_ p = void p <|> pure () ; {-# INLINE option'_ #-}
option_  p = option'_ $ try p   ; {-# INLINE option_  #-}

counted :: Monad m => m [a] -> m (Int, [a])
counted m = do
    as <- m
    return (length as, as)
{-# INLINE counted #-}

counted_ :: Monad m => m [a] -> m Int
counted_ = fmap2 fst counted ; {-# INLINE counted_ #-}

count :: Applicative m => Int -> m a -> m [a]
count i p = if i <= 0 then pure []
                      else (:) <$> p <*> count (pred i) p
{-# INLINE count #-}

fromJust :: MonadErrorParser SatisfyError m => Maybe a -> m a
fromJust = maybe (raise SatisfyError) return ; {-# INLINE fromJust #-}



--------------------------
-- === Char parsers === --
--------------------------

newline :: (Token m ~ Char, MonadTokenParser m, MonadProgressParser m, Alternative m, MonadThrowParser m, MonadErrorParser SatisfyError m, MonadErrorParser EmptyStreamError m) => m String
newline =  escape_n <|> escape_rn where
    escape_n  = fmap pure (token '\n')
    escape_rn = token '\r' <**> option pure ((\b a -> [a,b]) <$> token '\n')
{-# INLINE newline #-}



-------------------------------
-- tests

data OffsetError a = OffsetError Offset a deriving (Show)

instance (Monad m, Show t) => MonadErrorBuilder t m String where
    buildError t = return $ "ERROR: " <> show t ; {-# INLINE buildError #-}

instance (MonadState Offset m, MonadErrorBuilder t m a) => MonadErrorBuilder t m (OffsetError a) where
    buildError t = do
        off <- get @Offset
        OffsetError off <$> buildError t
    {-# INLINE buildError #-}


runTest1 :: IO (Either (NonEmpty String) [Char])
runTest1 = evalBacktracker
         $ runFailParser
         $ evalStreamProvider (listStream "babbbaabab")
         $ evalOffsetRegister
         $ (many (token 'a' <|> token 'b') )


runTest2 :: IO (Either (NonEmpty String) (Char, History Char))
runTest2 = runFailParser
         $ evalStreamProvider (listStream "babbbaabab")
         $ runHistoryRegister
         $ evalOffsetRegister
         $ ((token 'a' <|> token 'b') *> token 'a')

runTest3 :: IO (Either (NonEmpty String) Char)
runTest3 = evalBacktracker
         $ runFailParser
         $ evalStreamProvider (listStream "babbbaabab")
         $ evalOffsetRegister
         $ (try (token 'b' *> token 'b') <|> token 'b')

runTest4 :: IO (Either (NonEmpty (OffsetError String)) ())
runTest4 = evalBacktracker
         $ runFailParser @(OffsetError String)
         $ evalStreamProvider (listStream "abbbaabab")
         $ evalOffsetRegister
         $ recover_ (const dropToken) (token 'a' *> token 'a') <|> (() <$ token 'b')
        --  $ (token 'a' *> token 'a')

runTest5 :: IO (Either (NonEmpty String) Char)
runTest5 = evalBacktracker
         $ runFailParser
         $ evalStreamProvider (listStream "babbbaabab")
         $ evalOffsetRegister
         $ ((token 'b' *> (token 'x' <|> pure 'y') *> token 'z') <|> token 'b')

runTest6 :: IO (Either (NonEmpty String) Char)
runTest6 = evalBacktracker
         $ runFailParser
         $ evalStreamProvider (listStream "babbbaabab")
         $ evalOffsetRegister
         $ (try (token 'b' *> token 'z') <|> token 'b')

-- main :: IO ()
-- main = do
--     print =<< runTest2
--     print "---"

--
-- --
-- --
-- -- main :: IO ()
-- -- main = do
-- --     print "hello"
