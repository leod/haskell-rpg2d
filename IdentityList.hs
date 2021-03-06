module IdentityList
    ( IL
    , empty
    , insert, (+:)
    , delete
    , update
    , IdentityList.foldl
    , foldl'
    , toList
    , IdentityList.map
    , IdentityList.lookup
    , IdentityList.mapM
    , IdentityList.sequence
    , IdentityList.mapM_
    , IdentityList.sequence_
    ) where

import Control.Monad

type ILKey = Int

-- Descending order
data IL a = IL !ILKey ![(ILKey, a)]

instance (Show a) => Show (IL a) where
    show (IL _ as) = show as 

empty :: IL a
empty = IL 0 []

insert :: a -> IL a -> IL a
insert a (IL k as) =
    IL (k + 1) ((k, a) : as)

infixr 5 +:
(+:) :: a -> IL a -> IL a
(+:) = insert

delete :: ILKey -> IL a -> IL a
delete k (IL nk as) =
    IL nk (f as) 
    where f [] = []
          f as@(a@(k', _) : ks) | k > k'    = as
                                | k == k'   = ks
                                | otherwise = a : f ks

foldl :: (a -> (ILKey, b) -> a) -> a -> IL b -> a
foldl f z (IL k []) = z
foldl f z (IL k (a:as)) = IdentityList.foldl f (f z a) $ IL k as

foldl' :: (a -> (ILKey, b) -> a) -> a -> IL b -> a
foldl' f z (IL k as) = g f z as
    where g f z [] = z
          g f z (a:as) = let a' = f z a
                         in a' `seq` g f a' as

toList :: IL a -> [(ILKey, a)]
toList (IL _ as) = as

map :: ((ILKey, a) -> b) -> IL a -> IL b
map f (IL k as) =
    IL k [(k, f a) | a@(k, _) <- as]

sequence :: (Monad m) => IL (m a) -> m (IL a)
sequence (IL k as) = liftM (IL k) $ Prelude.mapM (\(k, v) -> liftM ((,) k) v) as

mapM :: Monad m => ((ILKey, a) -> m b) -> IL a -> m (IL b)
mapM f = IdentityList.sequence . IdentityList.map f

sequence_ :: (Monad m) => IL (m a) -> m ()
sequence_ (IL k as) = f as
    where f :: (Monad m) => [(ILKey, m a)] -> m ()
          f [] = return ()
          f ((_, x):xs) = x >> f xs 

mapM_ :: (Monad m) => ((ILKey, a) -> m b) -> IL a -> m ()
mapM_ f = IdentityList.sequence_ . IdentityList.map f

lookup :: ILKey -> IL a -> Maybe a
lookup k (IL _ as) = Prelude.lookup k as

update :: ILKey -> a -> IL a -> IL a
update k x = IdentityList.map f
    where f (k', x') | k == k'   = x
                     | otherwise = x'
