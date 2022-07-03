{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Tools.TestResult (TestResult(..), displayResult, finish, Timed, mkTimed, iterations, avgTime, totalTime, timed, result, NominalDiffTime) where

import Control.Applicative
import Control.Exception   (evaluate)
import Data.Time.Clock
import System.Exit         (die, exitSuccess)

newtype Iters = Iters Int
  deriving (Num, Eq, Ord, Show)

newtype TotalTime = TotalTime NominalDiffTime
  deriving (Num, Eq, Ord, Show)

data Timed a = Timed Iters TotalTime a
  deriving Show

instance Semigroup a => Semigroup (Timed a) where
  a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Timed a) where
  mempty = pure mempty

instance Functor Timed where
  fmap f (Timed isa tta a) = Timed isa tta (f a)

instance Applicative Timed where
  pure = Timed 0 0
  liftA2 f (Timed isa tta a) (Timed isb ttb b) = Timed (isa + isb) (tta + ttb) (f a b)

instance Monad Timed where
  return = pure
  Timed isa tta a >>= f = case f a of Timed isb ttb b -> Timed (isa + isb) (tta + ttb) b

totalTime :: Timed a -> NominalDiffTime
totalTime (Timed _ (TotalTime t) _) = t

avgTime :: Timed a -> NominalDiffTime
avgTime (Timed 0 _ _)                       = 0
avgTime (Timed (Iters isa) (TotalTime t) _) = t / fromIntegral isa

iterations :: Timed a -> Int
iterations (Timed (Iters isa) _ _) = isa

timed :: IO a -> IO (Timed a)
timed action = do
  start <- getCurrentTime
  r <- action >>= evaluate
  end <- r `seq` getCurrentTime
  return $ Timed 1 (TotalTime $ diffUTCTime end start) r

result :: Timed a -> a
result (Timed _ _ a) = a

mkTimed :: NominalDiffTime -> a -> Timed a
mkTimed t = Timed 1 (TotalTime t)

data TestResult
  = Success
  | Failure String
  deriving (Eq, Ord, Show, Read)


instance Semigroup TestResult where
  Success <> a           = a
  Failure s <> Success   = Failure s
  Failure s <> Failure t = Failure $ unlines [s, t]

instance Monoid TestResult where
  mempty = Success

displayResult :: Timed TestResult -> String
displayResult (Timed 0 _ Success) = "OK (no tests)."
displayResult t@(Timed 1 _ Success) = "OK (time: " ++ show (avgTime t) ++ ")."
displayResult t@(Timed _ _ Success) = "OK (avg time: " ++ show (avgTime t) ++ ")."
displayResult (Timed _ _ (Failure s)) = unlines $ ("Failure" :) . map (mappend "  ") . filter (not . null) $ lines s

finish :: TestResult -> IO a
finish Success     = exitSuccess
finish (Failure s) = die s
