module Tools.TestResult (TestResult(..), displayResult, finish) where

import           System.Exit (die, exitSuccess)

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

displayResult :: TestResult -> String
displayResult Success = "OK."
displayResult (Failure s) = unlines $ ("Failure" :) . map (mappend "  ") . filter (not . null) $ lines s

finish :: TestResult -> IO a
finish Success     = exitSuccess
finish (Failure s) = die s
