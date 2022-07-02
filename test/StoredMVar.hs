{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent                    (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Process.StoredMVar
import Tools.Runner
import Tools.TestResult

data BasicRole = Master | Slave
  deriving (Eq, Ord, Show, Read)

data ThreeWayRole = Reader | Taker | Putter
  deriving (Eq, Ord, Show, Read)

simpleTakePut :: TestSpec
simpleTakePut = Repeat 100 $ TestSpec "SimpleTakePut"
  [ (Master, run)
  , (Slave, run)
  ]
  where
    run :: BasicRole -> StoredMVar Double -> IO TestResult
    run Master mVar = do
      putMVar mVar 42
      putMVar mVar 17
      return Success
    run Slave mVar = do
      a <- takeMVar mVar
      b <- takeMVar mVar
      return $
        if (a + b) == (42 + 17)
          then Success
          else Failure $ show (a + b) ++ " /= 42 + 17"


readersTakers :: TestSpec
readersTakers = Repeat 100 $ TestSpec "ReadersTakers" $
    (, run) <$> (replicate 20 Reader <> [Taker, Putter])
  where
    run :: ThreeWayRole -> StoredMVar Int -> IO TestResult
    run Putter mVar = do
      let putLoud x = do
            putStrLn $ "Putting " ++ show x ++ "..."
            putMVar mVar x
      putLoud 177
      putLoud 178
      putLoud 179
      putLoud 777
      return Success
    run Taker mVar = do
      a <- takeMVar mVar
      b <- takeMVar mVar
      c <- takeMVar mVar
      putStrLn $ "Taking: " ++ show (a, b, c)
      return $
        if a < b && b < c
          then Success
          else Failure "Three taken numbers must go ordered!"
    run Reader mVar = do
      a <- readMVar mVar
      b <- readMVar mVar
      c <- readMVar mVar
      putStrLn $ "Reading: " ++ show (a, b, c)
      return $
        if a <= b && b <= c
          then Success
          else Failure "Three taken numbers must go ordered!"


asyncException :: TestSpec
asyncException = Repeat 10 $ WithTimeLimit 2000 $ TestSpec "AsyncException" [((), run)]
  where
    run :: () -> StoredMVar Int -> IO TestResult
    run _ mvar = do
      let timeMs = 50 :: Int
      locked <- async $ takeMVar mvar
      threadDelay $ timeMs * 1000
      killed <- async $ cancel locked
      let pollStatus t = do
            let t1 = t + timeMs
            threadDelay $ timeMs * 1000
            r <- poll killed
            case r of
              Nothing -> pollStatus t1
              Just _  -> return t1
      -- NB: the whole thread is gonna be killed by the test runner
      --     if pollStatus does not finish within the time defined in SpecParams.
      elapsed <- pollStatus 0
      putStrLn $ "Cancelled a thread within " ++ show elapsed ++ " ms"
      return Success

main :: IO ()
main = runTests [simpleTakePut, readersTakers, asyncException]
