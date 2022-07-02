{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent                    (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Process.StoredMVar
import Tools.Runner
import Tools.TestResult
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Control.Monad (when)
import System.Mem (performMajorGC)

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
asyncException = Repeat 10 $ TestSpec "AsyncException" [((), runA0)]

asyncException0 :: TestSpec
asyncException0 = TestSpec "AsyncException" [((), runA0)]

runA0 :: () -> StoredMVar Int -> IO TestResult
runA0 _ mvar = do
  putStrLn "Starting!"
  locked <- async $ takeMVar mvar
  putStrLn "Gonna delay"
  threadDelay (80000 :: Int)
  putStrLn "delayed"
  killed <- async $ cancel locked
  putStrLn "cancelled"
  threadDelay (350000 :: Int)
  putStrLn "Waited more"
  r <- poll killed
  putStrLn "Checked if killed"
  return $ case r of
    Nothing -> Failure "The thread did not finish in time."
    Just _  -> Success


main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    putStrLn ""
    newEmptyMVar >>= runA0 () >>= print
    hFlush stdout

  runTests [simpleTakePut, readersTakers, asyncException0, asyncException]
