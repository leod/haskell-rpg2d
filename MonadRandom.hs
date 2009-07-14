{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleInstances #-}
 
module MonadRandom (
    MonadRandom,
    getRandom,
    getRandomR,
    getRandoms,
    getRandomRs,
    evalRandT,
    evalRand,
    evalRandIO,
    runRandT,
    fromList,
    Rand, RandT -- but not the data constructors
    ) where
 
import System.Random
import Control.Monad.State.Strict
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Arrow
 
class (Monad m) => MonadRandom m where
    getRandom :: (Random a) => m a
    getRandoms :: (Random a) => m [a]
    getRandomR :: (Random a) => (a,a) -> m a
    getRandomRs :: (Random a) => (a,a) -> m [a]
 
newtype (RandomGen g) => RandT g m a = RandT (StateT g m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)
 
liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x
 
instance (Monad m, RandomGen g) => MonadRandom (RandT g m) where
    getRandom = RandT . liftState $ random
    getRandoms = RandT . liftState $ first randoms . split
    getRandomR (x,y) = RandT . liftState $ randomR (x,y)
    getRandomRs (x,y) = RandT . liftState $
                            first (randomRs (x,y)) . split
 
evalRandT :: (Monad m, RandomGen g) => RandT g m a -> g -> m a
evalRandT (RandT x) g = evalStateT x g
 
runRandT  :: (Monad m, RandomGen g) => RandT g m a -> g -> m (a, g)
runRandT (RandT x) g = runStateT x g
 
-- Boring random monad :)
newtype Rand g a = Rand (RandT g Identity a)
    deriving (Functor, Monad, MonadRandom)
 
evalRand :: (RandomGen g) => Rand g a -> g -> a
evalRand (Rand x) g = runIdentity (evalRandT x g)
 
runRand :: (RandomGen g) => Rand g a -> g -> (a, g)
runRand (Rand x) g = runIdentity (runRandT x g)
 
evalRandIO :: Rand StdGen a -> IO a
evalRandIO (Rand (RandT x)) = getStdRandom (runIdentity . runStateT x)
 
fromList :: (MonadRandom m) => [(a,Rational)] -> m a
fromList [] = error "MonadRandom.fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do let s = fromRational $ sum (map snd xs) -- total weight
                     cs = scanl1 (\(x,q) (y,s) -> (y, s+q)) xs -- cumulative weight
                 p <- liftM toRational $ getRandomR (0.0,s :: Double)
                 return . fst . head $ dropWhile (\(x,q) -> q < p) cs

instance (MonadRandom m) => MonadRandom (StateT s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs
 
instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs
 
instance (MonadRandom m) => MonadRandom (ReaderT r m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs
 
instance (MonadState s m, RandomGen g) => MonadState s (RandT g m) where
    get = lift get
    put = lift . put
 
instance (MonadReader r m, RandomGen g) => MonadReader r (RandT g m) where
    ask = lift ask
    local f (RandT m) = RandT $ local f m
 
instance (MonadWriter w m, RandomGen g, Monoid w) => MonadWriter w (RandT g m) where
    tell = lift . tell
    listen (RandT m) = RandT $ listen m
    pass (RandT m) = RandT $ pass m
