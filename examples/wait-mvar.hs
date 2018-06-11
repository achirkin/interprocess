{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Concurrent                   (forkOS) --  (threadDelay)
import           Control.Concurrent.Process.StoredMVar
import qualified Control.Concurrent.MVar as Vanilla
import           Control.Concurrent.QSemN
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

import Control.Exception (try, finally, SomeException)

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
    mVar <- do
      em <- try (newEmptyMVar :: IO (StoredMVar (Int, Double)))
      case em of
        Left e -> fail $ "Failed to create emv: " ++ show (e :: SomeException)
        Right r -> return r
    withNProcesses n processBConfig $ \procs -> do

      let mvName = mVarName mVar
      putStrLn $ "[A] Created mVar: " ++ show mvName

      forM_ (zip [99 :: Int, 98..] procs) $ \(i, p) -> do
        hPutSOName (getStdin p) mvName
        hPrint (getStdin p) $ 100 - i
        hFlush (getStdin p)

      -- threadDelay 100000
      putStrLn "[A] Done! Put lots of MVars for other threads"
      forM_ (zip [1 :: Int ..] procs) $ \(i,_) -> do
        -- threadDelay 10000 >>
        putStrLn $ "[A] Putting " ++ show i
        putMVar mVar (i, 1 / fromIntegral i) i
        putStrLn $ "[A] Have put " ++ show i

      -- finish with full mvar, so that all readers can finish too.
      -- tryReadMVar mVar >>= print
      -- putStrLn "[A] Want to Put last."
      -- putMVar mVar (0, 1/0)
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
  rr <- Vanilla.newEmptyMVar
  void . forkOS . flip finally (Vanilla.putMVar rr ()) $ do
    emVar <- try (lookupMVar mVarRef i :: IO (StoredMVar (Int, Double)))
    mr <- case emVar of
      Left e -> return $ Left e
      Right mVar -> try $ do
        let f :: Int -> IO ()
            f 0 = return ()
            f k = do
              putStrLn $ "[" ++ show i ++ "] " ++ show (1000 + k) ++ " Taking."
              (a,b) <- takeMVar mVar (1000 + k)
              seq a . seq b . putStrLn $ "[" ++ show i ++ "] " ++ show (1000 + k) ++ " Taken. Putting."
              putMVar mVar (a + 2, b * 2) (1000 + k)
              seq a . seq b . putStrLn $ "[" ++ show i ++ "] " ++ show (1000 + k) ++ " Have put."
              f (k - 1)
        f 10
        putStrLn $ "[" ++ show i ++ "] Taking last time."
        v <- takeMVar mVar 777
        putStrLn $ "[" ++ show i ++ "] (" ++ show v ++ ") Taken last time."
    print (mr :: Either SomeException ())
        -- print $ mVarName mVar
        -- if mod i 2 /= 3
        -- then do
        --   v <- readMVar mVar
        --   putStrLn $ "[B] (" ++ show i ++ ") Read first: " ++ show v
        -- else do
        --   threadDelay 200000
        --   v <- tryReadMVar mVar
        --   putStrLn $ "[B] (" ++ show i ++ ") TryRead first: " ++ show v
        -- threadDelay 500000
        -- if mod i 2 ==  1
        -- then
        --   -- let procedure k = do
        --   --       -- mval <- tryTakeMVar mVar
        --   --       mval <- (\x -> if k == 0 then Just x else Nothing) <$> takeMVar mVar
        --   --       case mval of
        --   --         Just v@(v1,v2) -> do
        --   --           putStrLn $ "[B] (" ++ show i ++ ") Was available: " ++ show v
        --   --           putMVar mVar (v1 + 1, v2 - 1)
        --   --         Nothing -> do
        --   --           putStrLn $ "[B] (" ++ show i ++ ") Ha-ha, I am not blocked!"
        --   --           v <- takeMVar mVar
        --   --           putStrLn $ "[B] (" ++ show i ++ ") Take one more: " ++ show v
        --   --           putMVar mVar (- i, fromIntegral $ i * i)
        --   --           -- threadDelay 100000
        --   --           procedure (k-1)
        --   -- in
        --   do
        --     putStrLn $ "[B] (" ++ show i ++ ") Starting take/put loop"
        --     -- procedure (5 :: Int)
        --     let f 0 = do
        --            putStrLn $ "[B] (" ++ show i ++ ") take the last one and we're done!"
        --            takeMVar mVar
        --         f k = do
        --            putStrLn $ "[B] (" ++ show i ++ ") take " ++ show k ++ " more"
        --            v@(a, b) <- takeMVar mVar
        --            putStrLn $ "[B] (" ++ show i ++ ") Took one more: " ++ show v
        --            putMVar mVar (a + 1, b * 2)
        --            f (k-1)
        --     v1@(a, b) <- f (10 :: Int)
        --     putStrLn $ "[B] (" ++ show i ++ ") Finished take/put loop!"
        --     putMVar mVar (a + 2, b - 1)
        --     v2 <- takeMVar mVar
        --     putStrLn $ "[B] (" ++ show i ++ ") Take and Take: " ++ show (v1, v2)
        -- else do
        --   putStrLn $ "[B] (" ++ show i ++ ") Want to Take."
        --   v <- takeMVar mVar
        --   putStrLn $ "[B] (" ++ show i ++ ") Take: " ++ show v
    putStrLn $ "[" ++ show i ++ "] Finished successfully"
  Vanilla.takeMVar rr
  putStrLn $ "[" ++ show i ++ "] Finished really successfully"

withNProcesses :: Int
              -> ProcessConfig stdin stdout stderr
              -> ([Process stdin stdout stderr] -> IO a)
              -> IO a
withNProcesses 0 _ k = k []
withNProcesses n conf k = do
  created <- newQSemN 0
  finished <- newQSemN 0
  done <- newQSemN 0
  procs <- Vanilla.newMVar []
  forM_ [1..n] $ \i ->
    void . forkOS $
      flip finally (signalQSemN finished 1) $
        withProcess_ conf $ \p -> do
          flip finally (signalQSemN created 1) $
            Vanilla.modifyMVar_ procs (pure . (p:))
          waitQSemN done 1
          waitExitCode p >>= putStrLn . ((show i ++ "-th process finished: " ) ++) . show


  finally ( waitQSemN created n >> Vanilla.takeMVar procs >>= k ) $ do
    signalQSemN done n
    putStrLn "Finished withNProcesses, waiting for procs to finish"
    waitQSemN finished n


instance Storable (Int, Double) where
    sizeOf _ = sizeOf (undefined :: Int) + sizeOf (undefined :: Double)
    alignment _ = 8
    peek ptr = (,) <$> peekByteOff ptr 0
                   <*> peekByteOff ptr (sizeOf (undefined :: Int))
    poke ptr (a, b) = pokeByteOff ptr 0 a
                   >> pokeByteOff ptr (sizeOf (undefined :: Int)) b
