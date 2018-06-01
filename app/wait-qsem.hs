module Main where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Process.QSem
import           Control.Monad
import           Data.List                       (partition)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (First (..))
import           Foreign.SharedObjectName
import           System.Environment
import           System.IO
import           System.Process.Typed
import           Text.Read                       (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let (isSlaveL, remargs) = partition ("slave"==) args
      isSlave = not $ null isSlaveL
      n = fromMaybe 4 . getFirst $ foldMap (First . readMaybe) remargs
  if isSlave
  then runB
  else runA n


runA :: Int -> IO ()
runA n = do
    progName <- getProgName
    args <- getArgs
    let processBConfig = setStdin createPipe
                       $ proc progName ("slave":args)

    withNProcesses n processBConfig $ \procs -> do

      qSem <- newQSem 0

      forM_ (zip [99 :: Int, 98..] procs) $ \(i, p) -> do
        hPutSOName (getStdin p) (qSemName qSem)
        hPrint (getStdin p) $ 100 - i
        hPutStrLn (getStdin p) $ "Say " ++ show i ++ " bottles of rum!"
        hFlush (getStdin p)

      threadDelay 100000
      putStrLn "[A] Done! Signal semaphore available to other threads"
      replicateM_ n $ threadDelay 100000 >> signalQSem qSem

    putStrLn "[A] Finished successfully"



runB :: IO ()
runB = do
    let inputH = stdin
    Just qSemRef <- hGetSOName inputH  -- get name of a semaphore
    qSem <- lookupQSem qSemRef
    i <- read <$> hGetLine inputH      -- get id of a spawned process
    instruction <- hGetLine inputH     -- some arbitrary text
    putStrLn $ "[B] " ++ instruction
    threadDelay 500000
    if mod i 7 == (2 :: Int)
    then
      let procedure = do
            wasAvailable <- tryWaitQSem qSem
            if wasAvailable
            then
              putStrLn $ "[B] (" ++ show i ++ ") was available - " ++ reverse instruction
            else do
              putStrLn $ "[B] (" ++ show i ++ ") Ha-ha, I am not blocked!"
              waitQSem qSem
              putStrLn $ "[B] (" ++ show i ++ ") Woke up"
              signalQSem qSem
              threadDelay 100000
              procedure
      in procedure
    else do
      waitQSem qSem
      putStrLn $ "[B] " ++ reverse instruction
    putStrLn "[B] Finished successfully"


withNProcesses :: Int
              -> ProcessConfig stdin stdout stderr
              -> ([Process stdin stdout stderr] -> IO a)
              -> IO a
withNProcesses 0 _ k = k []
withNProcesses n conf k = withProcess_ conf $ \p ->
    withNProcesses (n-1) conf (k . (p:))
