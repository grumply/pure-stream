{-# language ImplicitParams, ConstraintKinds, RankNTypes, OverloadedStrings #-}
module Pure.Stream (Step,step,stream,folds,unfolds,cons,nil,suspended,Stream()) where

import Pure.Stream.Internal as Stream hiding (step)
import qualified Pure.Stream.Internal as Stream

import Pure.Elm hiding (Step,step)
import qualified Pure.Intersection as I

import Data.Typeable
import qualified Data.List as List

type Step = (?step :: IO ())

step :: Step => IO ()
step = ?step

data Env state a = Env state (Step => state -> a -> (state,[View]))

newtype Model a = Model (Stream IO a)

data Msg = Step

{-# INLINE stream #-}
stream
  :: (Typeable state, Typeable a)
  => Stream IO a
  -> state
  -> (state -> a -> (state, [View]))
  -> View
stream = \s i f -> run (App [] [] [] (Model s) update view) (Env i f)
  where
    update _ _ (Model s) = Model <$> Stream.step s

    view (Env i f) (Model s) = let ?step = command Step in
      Div <||> 
        [ Div <||> List.concat
          ( Stream.folds (\_ -> []) (\_ _ -> []) 
            (\a g s -> let (s',vs) = f s a in vs : g s') 
            s i
          )
        , I.Observer def
          <| I.RootMargin "100px"
           . I.Threshold [0]
           . I.Action (\_ -> command Step)
        ]
