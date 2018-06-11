{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
module Main (main) where

import           Control.Concurrent       (forkOS, threadDelay)
import qualified Control.Concurrent.MVar  as Vanilla
import           Control.Concurrent.QSemN
import           Control.Exception
import           Control.Monad
import           Data.List                (partition)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (First (..))
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc    (alloca)
import           Foreign.Ptr
import           Foreign.Storable
import           System.Environment
import           System.IO
import           System.Process.Typed
import           Text.Read                (readMaybe)



main :: IO ()
main = do
  args <- getArgs
  let (isSlaveL, remargs) = partition ("slave"==) args
      isSlave = not $ null isSlaveL
      n = fromMaybe 2 . getFirst $ foldMap (First . readMaybe) remargs
  if isSlave
  then runB
  else runA n


runA :: Int -> IO ()
runA n = do
    execFile <- getExecutablePath
    args <- getArgs
    let processBConfig = setStdin createPipe
                       $ proc execFile ("slave":"+RTS":"-N4":"-RTS":
                                        args)

    putStrLn "[A] Started."
    mVar <- do
      em <- try (newEmptyMVar :: IO (StoredMVar Double))
      case em of
        Left e -> fail $ "Failed to create emv: " ++ show (e :: SomeException)
        Right r -> return r
    withNProcesses n processBConfig $ \procs -> do

      let mvName = mVarName mVar

      forM_ (zip [99 :: Int, 98..] procs) $ \(i, p) -> do
        hPutSOName (getStdin p) mvName
        hPrint (getStdin p) $ 100 - i
        hFlush (getStdin p)

      -- threadDelay 100000
      putStrLn "[A] Done! Put lots of MVars for other threads"
      forM_ (zip [1 :: Int ..] procs) $ \(i,_) -> do
        -- threadDelay 10000 >>
        putStrLn $ "[A] Putting " ++ show i
        putMVar mVar (1 / fromIntegral i) i
        putStrLn $ "[A] Have put " ++ show i
      putStrLn "[A] Have put all vals. Waiting"

    putStrLn "[A] Finished successfully"




runB :: IO ()
runB = do
  let inputH = stdin
  Just mVarRef <- hGetSOName inputH  -- get name of a semaphore
  i <- read <$> hGetLine inputH  :: IO Int    -- get id of a spawned process
  putStrLn $ "[B] (" ++ show i ++ ") Started."
  rr <- Vanilla.newEmptyMVar
  void . forkOS . flip finally (Vanilla.putMVar rr ()) $ do
    emVar <- try (lookupMVar mVarRef i :: IO (StoredMVar Double))
    mr <- case emVar of
      Left e -> return $ Left e
      Right mVar -> try $ do
        let f :: Int -> IO ()
            f 0 = return ()
            f k = do
              putStrLn $ "[" ++ show i ++ "] " ++ show (1000 + k) ++ " Taking."
              a <- takeMVar mVar (1000 + k)
              putStrLn $ "[" ++ show i ++ "] " ++ show (1000 + k) ++ " Taken. Putting."
              putMVar mVar (a + 1) (1000 + k)
              putStrLn $ "[" ++ show i ++ "] " ++ show (1000 + k) ++ " Have put."
              f (k - 1)
        f 10
        putStrLn $ "[" ++ show i ++ "] Taking last time."
        v <- takeMVar mVar 777
        putStrLn $ "[" ++ show i ++ "] (" ++ show v ++ ") Taken last time."
    putStrLn $ "[" ++ show i ++ "] Finished successfully (" ++ show (mr :: Either SomeException ()) ++ ")"
  let go k = do
        mr <- Vanilla.tryTakeMVar rr
        case mr of
          Nothing -> do
            threadDelay (1000000 * k)
            putStrLn $ "[" ++ show i ++ "] Was waiting the worker thread for " ++ show k ++ " sec"
            go (k * 2)
          Just () -> return ()
  go 1
  putStrLn $ "[" ++ show i ++ "] Finished really successfully"

-- spawn n processes from within n independent forkOSed threads.
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
          waitExitCode p >>= putStrLn . ((show (n+1-i) ++ "-th process finished: " ) ++) . show


  finally ( waitQSemN created n >> Vanilla.takeMVar procs >>= k ) $ do
    signalQSemN done n
    putStrLn "Finished withNProcesses, waiting for procs to finish"
    waitQSemN finished n


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
