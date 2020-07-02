{-# language RankNTypes, ScopedTypeVariables #-}
module Pure.Stream.Internal 
  ( Stream()
  , unfolds
  , folds
  , cons, nil, suspended
  , more, done
  , step, steps, force
  , stepSize, chunksOf
  , toList, toListM
  , fromList, fromListM
  ) where

import Control.Monad (join,(<$!>))
import Data.Monoid
import Data.Semigroup
import Data.Foldable hiding (toList)
import Data.Traversable
import Data.Function (fix)
import GHC.Exts (build)

data Stream f a = End | Suspend (f (Stream f a)) | Segment a (Stream f a)

{-# INLINE suspended #-}
suspended :: Functor f => f (Stream f a) -> Stream f a
suspended stream = builds $ \e c s -> c (fmap (folds e c s) stream)

{-# INLINE cons #-}
cons :: Functor f => a -> Stream f a -> Stream f a
cons a stream = builds $ \e c s -> s a (folds e c s stream)

{-# INLINE nil #-}
nil :: Stream f a
nil = builds $ \e c s -> e

instance Functor f => Monoid (Stream f a) where
  mempty = nil

instance Functor f => Semigroup (Stream f a) where
  (<>) = append

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
folds :: Functor f => b -> (f b -> b) -> (element -> b -> b) -> Stream f element -> b
folds e c s = go
  where
    go End = e
    go (Suspend fs) = c (fmap go fs)
    go (Segment a sa) = s a (go sa)
 
{-# INLINE unfolds #-}
unfolds :: Functor f => state -> (state -> f (Maybe (element, state))) -> Stream f element
unfolds initial f = 
  builds $ \e c s ->
    flip fix initial $ \loop st -> 
      let 
        unwrap (Just (a,st)) = s a (loop st)
        unwrap Nothing = e
      in 
        c (fmap unwrap (f st))

{-# INLINE more #-}
more :: Applicative f => element -> state -> f (Maybe (element,state))
more e s = pure (Just (e,s))

{-# INLINE done #-}
done :: Applicative f => f (Maybe (element,state))
done = pure Nothing

{-# RULES
"folds/builds" forall e c s (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
               folds e c s (builds f) = f e c s

  #-}

{-# INLINE step #-}
step :: Monad f => Stream f a -> f (Stream f a)
step = go
  where
    go (Segment a rest) = Segment a <$!> go rest
    go (Suspend fsa) = fsa
    go end = pure end

{-# INLINE uncons #-}
uncons :: Monad f => Stream f a -> f (Maybe (a,Stream f a))
uncons (Segment a s) = pure (Just (a,s))
uncons (Suspend f) = f >>= uncons
uncons End = pure Nothing

{-# INLINE steps #-}
steps :: Monad f => Int -> Stream f a -> f (Stream f a)
steps n stream = step $ force n stream

{-# INLINE force #-}
-- force `n` suspended streams
force :: Monad f => Int -> Stream f a -> Stream f a
force n stream
  | n <= 0    = stream
  | otherwise = 
    builds $ \e c s -> c $
      folds
        (\_ -> pure e)
        (\fns m ->
          if m == 0
            then pure $ c $ join $ fmap ($ 0) fns
            else join $ fmap ($ (m - 1)) fns
        )
        (\a stream n -> fmap (s a) $ stream n)
        stream
        n

{-# INLINE stepSize #-}
-- make `step` always force `n` suspended frames; subtly different than chunksOf
stepSize :: Monad f => Int -> Stream f a -> Stream f a
stepSize n stream
  | n <= 1    = stream
  | otherwise = 
    builds $ \e c s -> c $
      folds 
        (\_ -> pure e) 
        (\fns m ->
          if m == 0
            then pure $ c $ join $ fmap ($ (n - 1)) fns
            else join $ fmap ($ (m - 1)) fns 
        ) 
        (\a stream n -> fmap (s a) $ stream n)
        stream 
        n

{-# INLINE chunksOf #-}
-- make `step` always yield `n` elements; subtly different than stepSize
chunksOf :: Monad f => Int -> Stream f a -> Stream f a
chunksOf n stream
  | n <= 1    = stream
  | otherwise =  
    builds $ \e c s -> c $
      folds 
        (\_ -> pure e) 
        (\fns m ->
          if m == 0
            then pure $ c $ join $ fmap ($ n) fns
            else join $ fmap ($ m) fns 
        ) 
        (\a stream m -> 
          let m' | m == 0 = n | otherwise = m - 1
          in s a <$> stream m'
        )
        stream 
        n

{-# INLINE append #-}
append :: Functor f => Stream f a -> Stream f a -> Stream f a
append l r = builds $ \e c s -> folds (folds e c s r) c s l

{-
challenging (impossible?) without using constructors:

interleave :: Functor f => Stream f a -> Stream f a -> Stream f a
-}

{-# INLINE fromList #-}
fromList :: [a] -> Stream f a
fromList xs = 
  builds $ \e _ s ->
    foldr (\a rest -> s a rest) e xs

{-# INLINE fromListM #-}
fromListM :: Monad f => [f a] -> Stream f a
fromListM xs = 
  builds $ \e c s ->
    foldr (\x rest -> c (x >>= \a -> pure $ s a rest)) e xs

{-# INLINE toList #-}
toList :: Functor f => Stream f a -> [a]
toList xs = build $ \cons nil -> 
  folds 
    id 
    (\_ rest -> rest) 
    (\a c rest -> cons a (c rest)) 
    xs 
    nil

{-# INLINE toListM #-}
toListM :: Monad f => Stream f a -> f [a]
toListM xs = 
  folds 
    pure 
    (\fs rest -> join $ fmap ($ rest) fs) 
    (\a c rest -> fmap (a :) (c rest)) 
    xs 
    []