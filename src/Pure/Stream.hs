{-# language ImplicitParams, ConstraintKinds, RankNTypes, OverloadedStrings, 
      TemplateHaskell, RecordWildCards, ViewPatterns, PatternSynonyms,
      MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Pure.Stream (module Pure.Stream, Stream(), folds, unfolds, more, done, realized) where

import Pure.Stream.Internal as Stream hiding (step)
import qualified Pure.Stream.Internal as Stream

import Pure.Data.View (Pure(..))
import Pure.Elm hiding (Step,step,features,children)
import Pure.Data.Default
import Pure.Data.Prop.TH
import qualified Pure.Intersection as I

import Control.Monad
import Data.Typeable
import qualified Data.List as List

type Step = (?step :: IO ())

step :: Step => IO ()
step = ?step

data Env a = Env (Step => Streamer a)
data Model a = Model (Streamer a)

data Msg = Startup | Step

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
      let ?step = command Step
      in let s = streamer
         in pure (Model s)

    update _ _ (Model streamer) = do
      s' <- Stream.step (producer streamer) 
      pure $ Model streamer { producer = s' }

    view _ (Model Streamer_ {..}) = 
      Div <| SetFeatures features |> 
        ( Div <||> consumer producer 
        : children
        )

-- We can't write `instance (Step => Streamer a)`, so this will have to suffice
stepper :: Step => I.Observer
stepper = def & I.Threshold [0] & I.Action (\ts -> when (List.any I.intersecting ts) step)