{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Monad
import           Data.List             (partition)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (First (..))
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr
import           Foreign.Storable
import           System.Environment
import           System.IO
import           System.Process.Typed
import           Text.Read             (readMaybe)


-- | Execute runA by default, or execute runB if argument "slave" was supplied
--   Read one argument to determine how many slave processes to run
--
--   As usual, a few slave processes stuck on
--     [i] mvar_take 1 will wait 100, isFull 0, errno 2
--
--   This version looks simpler than the original one, but it requires more
--    processes to run simultaneously to exhibit the bug.
main :: IO ()
main = do
  args <- getArgs
  let (isSlaveL, remargs) = partition ("slave"==) args
      isSlave = not $ null isSlaveL
      n = fromMaybe 50 . getFirst $ foldMap (First . readMaybe) remargs
  if isSlave
  then runB
  else runA n


-- | spawn n processes and then send n mvars
runA :: Int -> IO ()
runA n = do
    execFile <- getExecutablePath
    args <- getArgs
    let processBConfig = setStdin createPipe
                       $ proc execFile ("slave":args)
    putStrLn "[0] Started."
    mVar <- newEmptyMVar :: IO (StoredMVar Double)
    -- spawn n processes
    withNProcesses n processBConfig $ \procs -> do

      -- send mvar name and process id to each of the processes
      forM_ (zip [1..] procs) $ \(i, p) -> do
        hPutSOName (getStdin p) (mVarName mVar)
        hPrint (getStdin p) (i :: Int)
        hFlush (getStdin p)
      putStrLn "[0] Put lots of MVars for other threads"
      -- putMVar n times
      forM_ (zip [1 :: Int ..] procs) $ \(i,_) -> do
        putStrLn $ "[0] Putting " ++ show i
        putMVar mVar (fromIntegral i) i
        putStrLn $ "[0] Have put " ++ show i
      putStrLn "[0] Have put all vals. Waiting"
    putStrLn "[0] Finished successfully"


-- | take and put mvar ten times; then take mvar last time.
runB :: IO ()
runB = do
    Just mVarRef <- hGetSOName stdin  -- get name of a semaphore
    i <- read <$> getLine :: IO Int   -- get id of a spawned process
    let pref = "[" ++ show i ++ "] "
    mVar <- lookupMVar mVarRef i :: IO (StoredMVar Double)
    putStrLn $ pref ++ "Started."
    takePutNTimes i mVar n
    putStrLn $ pref ++ "Taking last time."
    v <- takeMVar mVar (n+1)
    putStrLn $ pref ++ "(" ++ show v ++ ") Taken last time."
    putStrLn $ pref ++ "Finished successfully."
  where
    n = 10


takePutNTimes :: Int -> StoredMVar Double -> Int -> IO ()
takePutNTimes processId mVar n = go 1
  where
    pref = "[" ++ show processId ++ "] "
    go k | k == n    = return ()
         | otherwise = do
             putStrLn $ pref ++ show k ++ " Taking."
             a <- takeMVar mVar k
             putStrLn $ pref ++ show k ++ " Taken. Putting."
             putMVar mVar (a + 1) k
             putStrLn $ pref ++ show k ++ " Have put."
             go (k+1)


-- spawn n processes from within n independent forkOSed threads.
withNProcesses :: Int
              -> ProcessConfig stdin stdout stderr
              -> ([Process stdin stdout stderr] -> IO a)
              -> IO a
withNProcesses 0 _ k = k []
withNProcesses n conf k = withNProcesses (n-1) conf $ \ps ->
  withProcess_ conf $ k . (:ps)


data StoredMVarT

data StoredMVar a
  = StoredMVar !(SOName (StoredMVar a)) !(ForeignPtr StoredMVarT)


-- | Create a 'StoredMVar' which is initially empty.
newEmptyMVar :: forall a . Storable a => IO (StoredMVar a)
newEmptyMVar = do
    mvar <- checkNullPointer "newEmptyMVar"
          . c'mvar_new . fromIntegral $ sizeOf (undefined :: a)
    n <- newEmptySOName
    unsafeWithSOName n $ c'mvar_name mvar
    StoredMVar n <$> newForeignPtr p'mvar_destroy mvar
{-# NOINLINE newEmptyMVar #-}


-- | Find a `StoredMVar` created in another process ot thread by its reference.
lookupMVar :: Storable a => SOName (StoredMVar a) -> Int -> IO (StoredMVar a)
lookupMVar n i = do
    mvar <- unsafeWithSOName n $ checkNullPointer "lookupMVar"
     . flip c'mvar_lookup (fromIntegral i)
    StoredMVar n <$> newForeignPtr p'mvar_destroy mvar
{-# NOINLINE lookupMVar #-}

-- | Get a global reference to the `StoredMVar`.
--   Send this reference to another process to lookup this `StoredMVar` and
--   start interprocess communication.
mVarName :: StoredMVar a -> SOName (StoredMVar a)
mVarName (StoredMVar r _) = r


takeMVar :: Storable a => StoredMVar a -> Int -> IO a
takeMVar (StoredMVar _ fp) i = withForeignPtr fp $ \p -> alloca $ \lp -> do
    r <- c'mvar_take p lp $ fromIntegral i
    if r == 0
    then peek lp
    else do
      putStrLn $ "takeMVar failed with code " ++ show r
      throwErrno $ "takeMVar failed with code " ++ show r



putMVar :: Storable a => StoredMVar a -> a -> Int -> IO ()
putMVar (StoredMVar _ fp) x i = withForeignPtr fp $ \p -> alloca $ \lp -> do
    poke lp x
    r <- c'mvar_put p lp $ fromIntegral i
    if r == 0
    then return ()
    else do
      putStrLn $ "putMVar failed with code " ++ show r
      throwErrno $ "putMVar failed with code " ++ show r


checkNullPointer :: String -> IO (Ptr a) -> IO (Ptr a)
checkNullPointer s k = do
  p <- k
  if p == nullPtr
  then throwErrno (s ++ " returned NULL pointer.")
  else return p

foreign import ccall unsafe "mvar_new"
  c'mvar_new :: CSize -> IO (Ptr StoredMVarT)
foreign import ccall unsafe "mvar_lookup"
  c'mvar_lookup :: CString -> CInt -> IO (Ptr StoredMVarT)
foreign import ccall unsafe "&mvar_destroy"
  p'mvar_destroy :: FunPtr (Ptr StoredMVarT -> IO ())
foreign import ccall unsafe "mvar_name"
  c'mvar_name :: Ptr StoredMVarT -> CString -> IO ()
foreign import ccall interruptible "mvar_take"
  c'mvar_take :: Ptr StoredMVarT -> Ptr a -> CInt -> IO CInt
foreign import ccall interruptible "mvar_put"
  c'mvar_put :: Ptr StoredMVarT -> Ptr a -> CInt -> IO CInt



-- | Reference to a shared object; can be sent to other processes.
newtype SOName a = SOName (ForeignPtr CChar)


hPutSOName :: Handle -> SOName a -> IO ()
hPutSOName h (SOName q)
    = withForeignPtr q $ flip (hPutBuf h) 32


hGetSOName :: Handle -> IO (Maybe (SOName a))
hGetSOName h = do
    let n = 32
    q <- mallocForeignPtrBytes n
    n' <- withForeignPtr q $ \p -> hGetBuf h p n
    return $
      if n' < n
      then Nothing
      else Just (SOName q)


-- | Allocate a new shared object name.
newEmptySOName :: IO (SOName a)
newEmptySOName = SOName <$> mallocForeignPtrBytes 32


-- | Use a pointer to a C string to pass to some low-level (e.g. foreign) functions.
--   `SOName` is asserted immutable, so do not modify it!
unsafeWithSOName :: SOName a -> (CString -> IO b) -> IO b
unsafeWithSOName (SOName fp) = withForeignPtr fp
