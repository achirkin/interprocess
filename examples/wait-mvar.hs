{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Concurrent                    (threadDelay)
import           Control.Concurrent.Process.StoredMVar
import           Control.Monad
import           Data.List                             (partition)
import           Data.Maybe                            (fromMaybe)
import           Data.Monoid                           (First (..))
import           Foreign.SharedObjectName
import           Foreign.Storable
import           System.Environment
import           System.IO
import           System.Process.Typed
import           Text.Read                             (readMaybe)

import Control.Exception (try, SomeException)

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
    execFile <- getExecutablePath
    args <- getArgs
    let processBConfig = setStdin createPipe
                       $ proc execFile ("slave":args)

    putStrLn "[A] Started."
    withNProcesses n processBConfig $ \procs -> do

      mVar <- newEmptyMVar :: IO (StoredMVar (Int, Double))

      forM_ (zip [99 :: Int, 98..] procs) $ \(i, p) -> do
        hPutSOName (getStdin p) (mVarName mVar)
        hPrint (getStdin p) $ 100 - i
        hFlush (getStdin p)

      -- threadDelay 100000
      putStrLn "[A] Done! Put lots of MVars for other threads"
      forM_ (zip [1 :: Int ..] procs) $ \(i,_) ->
        -- threadDelay 10000 >>
        putMVar mVar (i, 1 / fromIntegral i)

      -- finish with full mvar, so that all readers can finish too.
      -- tryReadMVar mVar >>= print
      putStrLn "[A] Want to Put last."
      putMVar mVar (0, 1/0)
      putStrLn "[A] Have put. Waiting"
      -- threadDelay 100000
      -- putStrLn "[A] Waited. try to take mvar"
      -- tryTakeMVar mVar >>= print
      -- putStrLn "[A] Tried to take mvar"
      -- threadDelay 100000
      -- tryPutMVar mVar (0, 1) >> print (mVarName mVar)


    putStrLn "[A] Finished successfully"




runB :: IO ()
runB = do
    let inputH = stdin
    Just mVarRef <- hGetSOName inputH  -- get name of a semaphore
    i <- read <$> hGetLine inputH  :: IO Int    -- get id of a spawned process
    putStrLn $ "[B] (" ++ show i ++ ") Started."
    emVar <- try (lookupMVar mVarRef :: IO (StoredMVar (Int, Double)))
    case emVar of
      Left e -> print (e :: SomeException)
      Right mVar -> do
        print $ mVarName mVar
        -- if mod i 2 /= 3
        -- then do
        --   v <- readMVar mVar
        --   putStrLn $ "[B] (" ++ show i ++ ") Read first: " ++ show v
        -- else do
        --   threadDelay 200000
        --   v <- tryReadMVar mVar
        --   putStrLn $ "[B] (" ++ show i ++ ") TryRead first: " ++ show v
        -- threadDelay 500000
        -- if mod i 7 ==  9 -- 2
        -- then
        --   let procedure = do
        --         mval <- tryTakeMVar mVar
        --         case mval of
        --           Just v@(v1,v2) -> do
        --             putStrLn $ "[B] (" ++ show i ++ ") Was available: " ++ show v
        --             putMVar mVar (v1 + 1, v2 - 1)
        --           Nothing -> do
        --             putStrLn $ "[B] (" ++ show i ++ ") Ha-ha, I am not blocked!"
        --             v <- takeMVar mVar
        --             putStrLn $ "[B] (" ++ show i ++ ") Take one more: " ++ show v
        --             putMVar mVar (- i, fromIntegral $ i * i)
        --             threadDelay 100000
        --             procedure
        --   in do
        --     procedure
        --     v1 <- return "d" -- readMVar mVar
        --     v2 <- takeMVar mVar
        --     putStrLn $ "[B] (" ++ show i ++ ") Read and Take: " ++ show (v1, v2)
        -- else do
        putStrLn $ "[B] (" ++ show i ++ ") Want to Take."
        v <- takeMVar mVar
        putStrLn $ "[B] (" ++ show i ++ ") Take: " ++ show v
    putStrLn $ "[B] (" ++ show i ++ ") Finished successfully"


withNProcesses :: Int
              -> ProcessConfig stdin stdout stderr
              -> ([Process stdin stdout stderr] -> IO a)
              -> IO a
withNProcesses 0 _ k = k []
withNProcesses n conf k = withProcess_ conf $ \p ->
    withNProcesses (n-1) conf (k . (p:))


instance Storable (Int, Double) where
    sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: Double)
    alignment _ = 8
    peek ptr = (,) <$> peekByteOff ptr 0
                   <*> peekByteOff ptr (sizeOf (undefined :: Int))
    poke ptr (a, b) = pokeByteOff ptr 0 a
                   >> pokeByteOff ptr (sizeOf (undefined :: Int)) b
