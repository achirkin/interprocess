module Main where

import           Control.Monad
import           Data.List             (partition)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (First (..))
import           Foreign.Marshal.Alloc
import           Foreign.SharedPtr     as Shared
import           Foreign.Storable
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process.Typed
import           Text.Read             (readMaybe)
import           Data.IORef

-- | Supply integer argument to a program to set the allocation size.
---  Number 10000 is default, corresponds to argound 800MB of memory and very fast
--   Number 25000 corresponds to around 5GB of memory
--
-- > stack bench interprocess:concurrent-malloc --benchmark-arguments='25000'
main :: IO ()
main = do
  args <- getArgs
  let (isSlaveL, remargs) = partition ("slave"==) args
      isSlave = not $ null isSlaveL
      n = fromMaybe 10000 . getFirst $ foldMap (First . readMaybe) remargs
  if isSlave
  then runB n
  else runA n


runA :: Int -> IO ()
runA n = do
    execFile <- getExecutablePath
    args <- getArgs
    let processBConfig = setStdin createPipe
                       $ proc execFile ("slave":args)

    ec <- withNewAllocator $ \sa -> do

      putStrLn $ "[A] Allocator addr: " ++ show sa
      ptr <- Shared.malloc sa
      putStrLn $ "[A] Malloc'ed addr: " ++ show ptr
      poke ptr n

      ec' <- withProcess_ processBConfig $ \procB -> do

        putStrLn $ "[A] Sending store name: "
                  ++ show (allocStoreName sa)
        hPutStorable (getStdin procB) (allocStoreName sa)
        putStrLn $ "[A] Sending SharedPtr: "
                  ++ show (toSharedPtr sa ptr)
        hPutStorable (getStdin procB)
                          (toSharedPtr sa ptr)

        runMallocs "A" sa n

        putStrLn "[A] Now, waiting for process B to finish..."
        ec'' <- waitExitCode procB
        putStrLn "[A] Finished running B"
        return ec''
      n' <- peek ptr
      putStrLn $ "[A]  Read back: " ++ show n'
      Shared.free sa ptr
      putStrLn "[A]  Finished successfully"
      return ec'

    exitWith ec


runB :: Int -> IO ()
runB n = do
    let inputH = stdin
    Just sname <- hGetStorable inputH
    putStrLn $ "[B] Received allocator store name: " ++ show sname
    Just xptr <- hGetStorable inputH :: IO (Maybe (SharedPtr Int))
    putStrLn $ "[B] Received SharedPtr: " ++ show xptr

    withAllocator sname $ \sa -> do
      let ptr = fromSharedPtr sa xptr
      putStrLn $ "[B] Decoded SharedPtr: " ++ show ptr
      n' <- peek ptr
      putStrLn $ "[B] Compare N received through pipes (" ++ show n
               ++ ") and N read from shared memory (" ++ show n'
               ++ "): " ++ show (compare n n')
      poke ptr (n + 777)
      putStrLn $ "[B] updated pointer value: " ++ show (n + 777)

      runMallocs "B" sa ((n * 11) `div` 10)

    putStrLn "[B] Finished successfully"



-- | Run malloc on increasingly big size, 8 + i bytes, where i = [1..n].
runMallocs :: String -> Allocator -> Int -> IO ()
runMallocs runnerName a n = do
    reqBytes <- newIORef 0
    -- run malloc
    ptrs <- forM [1..n] $ \i -> do
      p <- Shared.mallocBytes a (8+i)
      modifyIORef' reqBytes (+(8+i))
      when (mod i (n `div` thisManyMallocFreeReports) == 7) $
        putStrLn $ "[" ++ runnerName ++ "] Malloced: " ++ show p
      forM_ [0 .. i `div` 8] $ \j -> pokeElemOff p j (i - j)
      return p
    -- run realloc
    ptrs' <- forM (zip [1..] ptrs)  $ \(i, p) -> do
      let j = newElemLength i
      p' <- Shared.realloc a p (8*j)
      modifyIORef' reqBytes (+(8*j))
      when (mod i (n `div` thisManyMallocFreeReports) == 7) $
        putStrLn $ "[" ++ runnerName ++ "] Realloced: " ++ show p'
      return p'

    -- run free
    r <- foldM (\(y, i) p -> do
      let j = newElemLength i
      -- generate some work so it needs time to finish
      x <- foldM (const $ peekElemOff p) 0 [j-1, j-2 .. 0]
      Shared.free a p
      when (mod i (n `div` thisManyMallocFreeReports) == 7) $
        putStrLn $ "[" ++ runnerName ++ "] Liberated ptr and read value: "
                 ++ show x
      return (y + x, i + 1)
      ) (0, 1 :: Int) ptrs'
    putStrLn $ "[" ++ runnerName ++ "] Validate results: "
            ++ show (r, sum [1..n])
    reqBytesVal <- readIORef reqBytes
    putStrLn $ "[" ++ runnerName ++ "] TotalMemory requested in the loops (MB): "
             ++ show (fromIntegral ((reqBytesVal * 1000 * 1000) `div` (1024 * 1024))
                          / (1000 * 1000) :: Double)
  where
    thisManyMallocFreeReports = 10
    newElemLength i = case mod i 9 of
      1 -> 3
      2 -> 1
      3 -> min 5 (div i 3)
      4 -> i + 7
      5 -> i * 2
      6 -> div (i * 3) 2
      _ -> i


hPutStorable :: Storable a => Handle -> a -> IO ()
hPutStorable h a = alloca $ \ptr -> do
  poke ptr a
  hPutBuf h ptr (sizeOf a)
  hFlush h

hGetStorable :: Storable a => Handle -> IO (Maybe a)
hGetStorable h = go undefined
  where
    go :: Storable a => a -> IO (Maybe a)
    go a = alloca $ \ptr -> do
      let n = sizeOf a
      n' <- hGetBuf h ptr n
      if n' < n
      then return Nothing
      else Just <$> peek ptr
