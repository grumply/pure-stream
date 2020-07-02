{-# language ImplicitParams, ConstraintKinds, RankNTypes, OverloadedStrings, 
      TemplateHaskell, RecordWildCards, ViewPatterns, PatternSynonyms,
      MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Pure.Stream 
  ( module Pure.Stream
  , Stream()
  , folds
  , unfolds
  , more, done
  , force, stepSize, chunksOf
  , toList, toListM
  , fromList, fromListM
  , append, concat
  , repeat, repeatM
  , infinite
  , take, drop
  , null
  , head, headM
  , headMay, headMayM
  , tail
  , reverse
  ) where

import Pure.Stream.Internal as Stream hiding (step,steps)
import qualified Pure.Stream.Internal as Stream

import Pure.Data.View (Pure(..))
import Pure.Elm hiding (Step,step,features,children,force,reverse,head,drop,infinite,repeat)
import Pure.Data.Default
import Pure.Data.Prop.TH
import qualified Pure.Intersection as I

import Control.Monad
import Data.Typeable
import qualified Data.List as List
import Prelude hiding (concat,repeat,take,drop,null,head,tail,reverse,init,zip,zipWith,length)

type Step = (?step :: Int -> IO ())

step :: Step => IO ()
step = ?step 1

steps :: Step => Int -> IO ()
steps = ?step

data Env a = Env (Step => Streamer a)
data Model a = Model (Streamer a)

data Msg = Startup | Step Int

data Streamer a = Streamer_
  { producer :: Stream IO a
  , consumer :: Stream IO a -> [View]
  , features :: Features
  , children :: [View]
  }

instance Default (Streamer a) where
  def = Streamer_ nil (const []) def def

deriveLocalComponent ''Streamer

{-# INLINE stream #-}
stream :: Typeable a => (Step => Streamer a) -> View
stream s = run (App [Startup] [] [] (Model def) update view) (Env s)
  where
    update Startup (Env streamer) _ = 
      let ?step = command . Step
      in let s = streamer
         in pure (Model s)

    update (Step n) _ (Model streamer) = do
      s' <- Stream.steps n (producer streamer) 
      pure $ Model streamer { producer = s' }

    view _ (Model Streamer_ {..}) = 
      Div <| SetFeatures features |> 
        ( Div <||> consumer producer 
        : children
        )

-- We can't write `instance (Step => Streamer a)`, so this will have to suffice
stepper :: Step => I.Observer
stepper = def & I.Threshold [0] & I.Action (\ts -> when (List.any I.intersecting ts) step)