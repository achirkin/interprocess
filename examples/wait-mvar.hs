{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Concurrent.Process.StoredMVar
import           Control.Exception                     (SomeException, catch,
                                                        displayException)
import           Control.Monad
import           Data.List                             (partition)
import           Data.Maybe                            (fromMaybe)
import           Data.Monoid                           (First (..))
import           Foreign.SharedObjectName
import           GHC.Environment                       (getFullArgs)
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process.Typed
import           Text.Read                             (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let (isSlaveL, remargs) = partition ("slave"==) args
      isSlave = not $ null isSlaveL
      i = fromMaybe 1 . getFirst $ foldMap (First . readMaybe) remargs
  if isSlave
  then runB i `catch` ( \e -> do
                          putStrLn $ "[" ++ show i ++ "] "
                                            ++ displayException (e :: SomeException)
                          exitFailure
                      )
  else runA i `catch` ( \e -> do
                          putStrLn $ "[A] " ++ displayException (e :: SomeException)
                          exitFailure
                      )

runA :: Int -> IO ()
runA n = do
    execFile <- getExecutablePath
    args <- getFullArgs
    let pconfs = flip map [1..n] $ \i -> setStdin createPipe
                                 $ proc execFile ("slave" : show i : args)
    mVar <- newEmptyMVar :: IO (StoredMVar Double)

    withProcesses pconfs $ \procs -> do

      let mvName = mVarName mVar
      report $ "Created mVar: " ++ show mvName

      -- send name of StoredMVar to every slave process
      forM_ procs $ \p -> do
        hPutSOName (getStdin p) mvName
        hFlush (getStdin p)

      report "Sent MVar name. Now putMVar n times"
      forM_ (zipWith const [1..] procs) $ \i -> do
        let x = recip i
        report $ "Putting " ++ show x
        putMVar mVar x
        report $ "Have put " ++ show x ++ " (" ++ show i ++ "-th)"

      report "Have put. Waiting"

    report "Finished successfully"
  where
    report s =  putStrLn $ "[A] " ++ s


runB :: Int -> IO ()
runB i = do
    Just mVarRef <- hGetSOName stdin  -- get name of a StoredMVar
    report "Started."

    mVar <- lookupMVar mVarRef :: IO (StoredMVar Double)
    takePutMTimes mVar 20
    report "Taking last time."
    v <- takeMVar mVar
    report $ show v ++ " Taken last time."
    report "Finished successfully"
  where
    takePutMTimes mVar m = go (1 :: Int)
      where
        go k | k > m = return ()
             | mod k 5 == 3 = do
                report $ show k ++ " Swapping."
                a <- swapMVar mVar 17.5
                report $ show k ++ " Have swapped " ++ show a
                go (k+1)
             | mod k 3 == 2 = do
                report $ show k ++ " Reading."
                a <- readMVar mVar
                report $ show k ++ " Have read " ++ show a
                go (k+1)
             | mod k 7 == 6 = do
                report $ show k ++ " Try taking."
                ma <- tryTakeMVar mVar
                case ma of
                  Just a -> do
                    report $ show k ++ " Trytaken succesfully. Try putting."
                    putSuccess <- tryPutMVar mVar (a + 3)
                    if putSuccess
                      then do
                        report $ show k ++ " I'm lucky! Try swapping now."
                        mb <- trySwapMVar mVar (a - 7)
                        case mb of
                          Just b -> do
                            report $ show k ++ " Bingo! " ++ show (a, b)
                            go (k+1)
                          Nothing -> do
                            report $ show k ++ " Failed to tryswap."
                            go (k+1)
                      else do
                        report $ show k ++ " Could not tryput. Wait and put now."
                        putMVar mVar (a + 1)
                        go (k+1)
                  Nothing -> do
                    report $ show k ++ " Could not trytake. Repeat."
                    go k
             | otherwise = do
                report $ show k ++ " Taking."
                a <- takeMVar mVar
                report $ show k ++ " Taken " ++ show a ++ ". Putting."
                putMVar mVar (a * 2)
                report $ show k ++ " Have put."
                go (k+1)

    report s =  putStrLn $ "[" ++ show i ++ "] " ++ s



withProcesses ::  [ProcessConfig stdin stdout stderr]
              -> ([Process stdin stdout stderr] -> IO a)
              -> IO a
withProcesses [] k = k []
withProcesses (conf:cfs) k = withProcesses cfs $ \ps -> withProcess_ conf $ k . (:ps)
