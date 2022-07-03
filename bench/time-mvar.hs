module Main (main) where

import Control.Concurrent.Process.StoredMVar
import Control.Monad
import Tools.Runner
import Tools.TestResult

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar  as Vanilla

data Role = Putter | Taker
  deriving (Eq, Ord, Show, Read)

benchIPC :: Int -> Int -> TestSpec
benchIPC takers repeats = TestSpec ("interprocess take/put (" ++ show takers ++ " takers / " ++ show repeats ++ " times)")
  ((Putter, run) : replicate takers (Taker, run))
  where
    run :: Role -> StoredMVar Int -> IO TestResult
    run Putter mVar = do
      forM_ [1..(repeats * takers)] $ putMVar mVar
      return Success
    run Taker mVar = check <$> foldM f 0 [1..repeats]
      where
        check r = if r <= repeats then Success else Failure "impossible"
        f s i = (\j -> s + signum (i - j)) <$> takeMVar mVar

benchVanilla :: Int -> Int -> TestSpec
benchVanilla takers repeats = TestSpec ("vanilla take/put (" ++ show takers ++ " takers / "  ++ show repeats ++ " times)")
  [ ((), run')
  ]
  where
    run' :: () -> StoredMVar Int -> IO TestResult
    run' () _ = do
      mv <- Vanilla.newEmptyMVar
      Async.runConcurrently $ foldMap Async.Concurrently $ run Putter mv : replicate takers (run Taker mv)
    run Putter mVar = do
      forM_ [1..(repeats * takers)] $ Vanilla.putMVar mVar
      return Success
    run Taker mVar = check <$> foldM f 0 [1..repeats]
      where
        check r = if r <= repeats then Success else Failure "impossible"
        f s i = (\j -> s + signum (i - j)) <$> Vanilla.takeMVar mVar

main :: IO ()
main = do
  defRunnerParams <- defaultSpecParams
  let runnerParams = defRunnerParams { timeout = 200000 }
      repeatsIPC = 100000
      repeatsVanilla = 1000000
      takersNums = [1, 2, 4, 8, 16, 32, 64]
      testRepeats = [10, 10, 10, 5, 1, 1, 1]
      testsIPC = zipWith (\takers tr -> Repeat tr (benchIPC takers repeatsIPC)) takersNums testRepeats
      testsVanilla = zipWith (\takers tr -> Repeat tr (benchVanilla takers repeatsVanilla))  takersNums testRepeats

  results <- runTests runnerParams (testsIPC ++ testsVanilla)

  let (resultsIPC, resultsVanilla) = splitAt (length testsIPC) results
      analyze takers ripc rv
        | Success <- result ripc
        , Success <- result rv = do
            let tipc = avgTime ripc / fromIntegral repeatsIPC
                tv = avgTime rv / fromIntegral repeatsVanilla
                slowDown = realToFrac (tipc / tv) :: Double
            putStrLn $ "Takers: " ++ show takers
            putStrLn $ "  interprocess time: " ++ show tipc
            putStrLn $ "       vanilla time: " ++ show tv
            putStrLn $ "       IPC slowdown: " ++ show slowDown
      analyze takers _ _ = putStrLn $ "Takers: " ++ show takers ++ " -- Failure"

  sequence_ $ zipWith3 analyze takersNums resultsIPC resultsVanilla
