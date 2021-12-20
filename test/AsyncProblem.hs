module Main (main) where

import Control.Concurrent                    (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Process.StoredMVar
import System.Exit

main :: IO ()
main = do
  locked <- async (takeMVar =<< newEmptyMVar :: IO Int)
  threadDelay (55000 :: Int)
  killed <- async $ cancel locked
  threadDelay (55000 :: Int)
  r <- poll killed
  case r of
    Nothing -> exitFailure
    Just _  -> exitSuccess
