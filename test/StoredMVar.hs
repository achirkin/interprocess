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
asyncException = Repeat 10 $ TestSpec "AsyncException" [((), run)]
  where
    run :: () -> StoredMVar Int -> IO TestResult
    run _ mvar = do
      putStrLn "Starting!"
      locked <- async $ putStrLn "started async" >> takeMVar mvar
      putStrLn "Gonna delay"
      threadDelay (55000 :: Int)
      putStrLn "delayed"
      killed <- async $ cancel locked
      putStrLn "cancelled"
      threadDelay (55000 :: Int)
      putStrLn "Waited more"
      r <- poll killed
      putStrLn "Checked if killed"
      return $ case r of
        Nothing -> Failure "The thread did not finish in time."
        Just _  -> Success

main :: IO ()
main = runTests [simpleTakePut, readersTakers, asyncException]
