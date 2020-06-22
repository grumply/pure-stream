{-# language RankNTypes #-}
module Pure.Stream.Internal (Stream(), unfolds, folds, step) where

import Data.Foldable
import Data.Traversable
import Data.Function (fix)

data Stream f a = End | Suspend (f (Stream f a)) | Segment a (Stream f a)

instance Functor f => Functor (Stream f) where
  {-# INLINE fmap #-}
  fmap f xs = builds $ \e c s -> folds e c (s . f) xs

instance (Functor f, Foldable f) => Foldable (Stream f) where
  {-# INLINE foldr #-}
  foldr f st = folds st (foldr const undefined) f

instance (Functor f, Traversable f) => Traversable (Stream f) where
  {-# INLINE traverse #-}
  traverse f = go
    where
      go End = pure End
      go (Suspend fs) = Suspend <$> traverse go fs
      go (Segment a fs) = Segment <$> f a <*> go fs

{-# INLINE [1] builds #-}
builds :: (forall b. b -> (f b -> b) -> (a -> b -> b) -> b) -> Stream f a
builds f = f End Suspend Segment

{-# INLINE [1] folds #-}
folds :: Functor f => b -> (f b -> b) -> (a -> b -> b) -> Stream f a -> b
folds e c s = go
  where
    go End = e
    go (Suspend fs) = c (fmap go fs)
    go (Segment a sa) = s a (go sa)

{-# INLINE unfolds #-}
unfolds :: Functor f => (state -> f (Maybe (a, state))) -> state -> Stream f a
unfolds f initial = 
  builds $ \e c s ->
    flip fix initial $ \loop st -> 
      let 
        unwrap (Just (a,st)) = s a (loop st)
        unwrap Nothing = e
      in 
        c (fmap unwrap (f st))

{-# RULES
"folds/builds" forall e c s (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
               folds e c s (builds f) = f e c s
  #-}

{-# INLINE step #-}
step :: Monad f => Stream f a -> f (Stream f a)
step (Segment a rest) = Segment a <$> step rest
step (Suspend fsa) = fsa
step end = pure end