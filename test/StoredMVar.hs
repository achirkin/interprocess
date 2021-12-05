{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Concurrent                    (forkIO)
import qualified Control.Concurrent.MVar               as Vanilla
import           Control.Concurrent.Process.StoredMVar
import           Control.Exception
import           Control.Monad                         (forM, void)
import           Data.Monoid
import           Foreign.SharedObjectName
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           System.Mem
import           System.Process.Typed
import           Text.Read                             (readMaybe)

-- | A number of processes trying to do something concurrently.
--   For example, read from the same StoredMVar.
--   Actual number of processes may vary depending on a test case.
--   Minimum allowed number is 1.
concProcBaseN :: Int
concProcBaseN = 20

data BasicRole = Master | Slave
  deriving (Eq, Ord, Show, Read)

data ThreeWayRole = Reader | Taker | Putter
  deriving (Eq, Ord, Show, Read)

data TestSpec
  = SimpleTakePut (SOName (StoredMVar Double)) BasicRole
  | ReadersTakers (SOName (StoredMVar Double)) ThreeWayRole
  deriving (Eq, Ord, Show, Read)


run :: TestSpec -> IO TestResult

run (SimpleTakePut ref Master) = do
  mVar <- lookupMVar ref
  putMVar mVar 42
  putMVar mVar 17
  return Success
run (SimpleTakePut ref Slave) = do
  mVar <- lookupMVar ref
  a <- takeMVar mVar
  b <- takeMVar mVar
  return $ if (a + b) == (42 + 17)
           then Success
           else Failure $ show (a + b) ++ " /= 42 + 17"

run (ReadersTakers ref Putter) = do
  mVar <- lookupMVar ref
  putMVar mVar 177
  putMVar mVar 178
  putMVar mVar 179
  putMVar mVar 777
  return Success
run (ReadersTakers ref Taker) = do
  mVar <- lookupMVar ref
  a <- takeMVar mVar
  b <- takeMVar mVar
  c <- takeMVar mVar
  putStrLn $ "Taking: " ++ show (a,b,c)
  return $ if a < b && b < c
           then Success
           else Failure "Three taken numbers must go ordered!"
run (ReadersTakers ref Reader) = do
  mVar <- lookupMVar ref
  a <- readMVar mVar
  b <- readMVar mVar
  c <- readMVar mVar
  putStrLn $ "Reading: " ++ show (a,b,c)
  return $ if a <= b && b <= c
           then Success
           else Failure "Three taken numbers must go ordered!"

tests :: [IO ([TestSpec], IO ())]
tests =
  [ do
    mVar <- newEmptyMVar
    return
      ( [ SimpleTakePut (mVarName mVar) Slave
        , SimpleTakePut (mVarName mVar) Master
        ]
      , return (mVar `seq` ())
      )
  , do
    mVar <- newEmptyMVar
    return
      ( replicate concProcBaseN (ReadersTakers (mVarName mVar) Reader)
        ++
        [ ReadersTakers (mVarName mVar) Taker
        , ReadersTakers (mVarName mVar) Putter
        ]
      , return (mVar `seq` ())
      )
  ]



data TestResult
  = Success
  | Failure String
  deriving (Eq, Ord, Show, Read)

instance Semigroup TestResult where
  Success <> a           = a
  Failure s <> Success   = Failure s
  Failure s <> Failure t = Failure $ unlines [s,t]

instance Monoid TestResult where
  mempty = Success

displayResult :: TestResult -> String
displayResult Success = "OK."
displayResult (Failure s) = unlines $ ("Failure":) . map (mappend "  ") . filter (not . null) $ lines s

finish :: TestResult -> IO a
finish Success     = exitSuccess
finish (Failure s) = die s

main :: IO ()
main = do
    execFile <- getExecutablePath
    args <- getArgs
    case getFirst $ foldMap (First . readMaybe) args of
      -- run particular test routine
      Just spec -> run spec >>= finish
      -- run all tests
      Nothing -> do
        results <- forM tests $ \iox -> do
          (ts, fin) <- iox
          r <-runSpecs execFile ts
          fin
          return r
        case foldMap id results of
          Success   -> exitSuccess
          Failure _ -> exitFailure

runSpecs :: FilePath -> [TestSpec] -> IO TestResult
runSpecs f specs = do
    putStrLn ""
    r <- go (zip [1 :: Int ..] specs) >>= evaluate
    performGC
    putStrLn $ "Result: " ++ displayResult r
    return r
  where
    conf ts = setStdout createPipe
            $ setStderr createPipe
            $ proc f [show ts]

    go [] = return Success
    go ((i,x):xs) = do
        mr <- Vanilla.newEmptyMVar
        void . forkIO $ withProcessWait (conf x) $ \p -> do
          hGetContents (getStdout p) >>= mapM_ (putStrLn . withI) . lines
          errs <- hGetContents (getStderr p)
          ecode <- waitExitCode p
          evaluate $ foldr seq () errs
          Vanilla.putMVar mr $! case ecode of
            ExitSuccess   -> Success
            ExitFailure _ -> Failure (unlines . map withI $ lines errs)

        rx <- unsafeInterleaveIO $ Vanilla.takeMVar mr
        rxs <- go xs
        return $ mappend rx rxs
      where
        withI s = "[" ++ show i ++ "] " ++ s
