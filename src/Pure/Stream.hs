{-# language ImplicitParams, ConstraintKinds, RankNTypes, OverloadedStrings, 
      TemplateHaskell, RecordWildCards, ViewPatterns, PatternSynonyms,
      MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Pure.Stream (module Pure.Stream, module Stream) where

import Pure.Stream.Internal as Stream hiding (step)
import qualified Pure.Stream.Internal as Stream

import Pure.Elm hiding (Step,step,features,children)
import Pure.Data.Default
import Pure.Data.Prop.TH

import Data.Typeable
import qualified Data.List as List

type Step = (?step :: IO ())

step :: Step => IO ()
step = ?step

data Env state a = Env (Step => Streamer state a)
data Model state a = Model (Streamer state a) (Stream IO a)

data Msg = Startup | Step

data Streamer state a = Streamer_
  { producer :: Stream IO a
  , features :: Features
  , children :: [View]
  , initial  :: state
  , consumer :: state -> a -> (state,[View])
  }

instance Default state => Default (Streamer state a) where
  def = Streamer_ nil def def def (\st _ -> (st,[]))

deriveLocalComponent ''Streamer

{-# INLINE stream #-}
stream :: (Typeable state, Typeable a) => (Step => Streamer state a) -> View
stream s = run (App [Startup] [] [] (Model undef nil) update view) (Env s)
  where
    undef = Streamer_ undefined undefined undefined undefined undefined

    update Startup (Env streamer) (Model _ _) = 
      let ?step = command Step
      in let s = streamer
         in pure (Model s (producer s))

    update _ _ (Model streamer s) = 
      Model streamer <$> Stream.step s

    view _ (Model Streamer_ {..} s) = 
      -- Any case in which this shouldn't be a block-level component?
      Div <| SetFeatures features |> 
        ( (Div <||> List.concat
            ( Stream.folds (\_ -> []) (\_ _ -> []) 
              (\a g s -> let (s',vs) = consumer s a in vs : g s') 
              s initial
            )
          )
        : children
        )
