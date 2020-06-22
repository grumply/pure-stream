{-# language ImplicitParams, ConstraintKinds, RankNTypes #-}
module Pure.Stream (Step,step,stream,folds,unfolds,Stream()) where

import Pure.Stream.Internal as Stream hiding (step)
import qualified Pure.Stream.Internal as Stream

import Pure.Elm hiding (Step,step)
import qualified Pure.Visibility as V

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
        , V.Visibility def
          <| V.Once False
          .  V.FireOnMount True
          .  V.OnOnScreen (Just $ const (command Step))
        ]
