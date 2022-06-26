{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tools.Runner (runTests, TestSpec (..)) where

import           Control.Concurrent                    (threadDelay)
import           Control.Concurrent.Async
import qualified Control.Concurrent.MVar               as StdMVar
import           Control.Concurrent.Process.StoredMVar
import           Control.Exception                     (SomeException, catch,
                                                        evaluate)
import           Control.Monad                         (foldM, forever, when)
import           Control.Monad.STM                     (atomically)
import qualified Data.ByteString.Char8                 as BS
import           Data.ByteString.Lazy.Char8            (unpack)
import           Data.Foldable                         (fold)
import           Data.IORef                            (atomicModifyIORef',
                                                        newIORef)
import qualified Data.List                             as List
import qualified Data.Map                              as Map
import           Data.Tuple                            (swap)
import           Foreign.Storable                      (Storable)
import           System.Environment                    (getArgs,
                                                        getExecutablePath)
import           System.Exit
import           System.IO
import           System.Mem                            (performMajorGC)
import           System.Process.Typed
import           Text.Read                             (readMaybe)

import Tools.Handle
import Tools.TestResult

{- |
  Specification of a single test.
  Defines the test name and the list of roles to be given to subprocesses.

  The test spawns as many subprocesses as there are elements in the roles list.
  All of them share a single IPC MVar for communication. In addition, the role
  actions are parameterized by the role key `k`, which is passed via the command-line
  arguments using the Show-Read instances.
 -}
data TestSpec where
  TestSpec :: forall k ctx
            . (Show k, Read k, Eq k, Storable ctx)
           => String -> [(k, k -> StoredMVar ctx -> IO TestResult)] -> TestSpec
  -- | Set the maximum time the test can take in milliseconds.
  --   The test process will be forcefully terminated after the time limit elapses.
  WithTimeLimit :: Int -> TestSpec -> TestSpec
  -- | Repeat the test the specified number of times.
  Repeat :: Int -> TestSpec -> TestSpec

-- | Get the name of the test.
testName :: TestSpec -> String
testName (TestSpec name _)   = name
testName (WithTimeLimit _ s) = testName s
testName (Repeat _ s)        = testName s

stripSpec :: TestSpec -> TestSpec
stripSpec (WithTimeLimit _ s) = stripSpec s
stripSpec (Repeat _ s)        = stripSpec s
stripSpec s                   = s

data SpecParams = SpecParams
  { exec                :: String -- ^ Executable name
  , timeout             :: Int -- ^ Terminate process after this amount of milliseconds
  , processStartTimeout :: Int -- ^ Fail if the child process not all child processes start within this time
  }

defaultSpecParams :: String -> SpecParams
defaultSpecParams execFile = SpecParams
  { exec = execFile
  , timeout = 1000
  , processStartTimeout = 10000
  }

-- | A random string for a handshake between the processes.
startToken :: String
startToken = "KxmpaDIlwnSlfp01==123=123=1"

-- | Run all tests in the list. Use it as the `main`:
--
-- @
--   main :: IO ()
--   main = runTests [...]
-- @
runTests :: [TestSpec] -> IO ()
runTests specs = do
  execFile <- getExecutablePath
  args <- getArgs
  case args of
    -- run all tests
    [] -> do
        results <- mapM (runSpec $ defaultSpecParams execFile) specs
        case fold results of
            Success   -> exitSuccess
            Failure _ -> exitFailure

    -- run particular test routine
    [n, sk, sm] -> do
        hSetBuffering stdout LineBuffering
        putStrLn startToken
        returnedToken <- getLine
        when (startToken /= returnedToken) $ fail "Child start handshake failed."
        TestSpec _ roles <- case List.find ((== n) . testName) specs of
            Nothing   -> fail $ "Unknown test name: " ++ show n
            Just spec -> pure $ stripSpec spec
        k <- case readMaybe sk of
            Nothing -> fail $ "Couldn't read the role " ++ show sk
            Just x  -> pure x
        m <- case readMaybe sm of
            Nothing  -> fail $ "Couldn't read the mvar name " ++ show sm
            Just ref -> lookupMVar ref
        case List.find ((== k) . fst) roles of
            Nothing -> fail $ "Unknown test role " ++ show k ++ " for test " ++ show n
            Just (_ , action) -> action k m >>= finish

    -- unexpected
    _ -> fail $ "Expected zero or three arguments, got " ++ show args


runSpec :: SpecParams -> TestSpec -> IO TestResult
runSpec pams (WithTimeLimit t s) = runSpec (pams {timeout = t}) s
runSpec pams (Repeat n s) = do
    putStrLn $ "[" ++ testName s ++ "] Running " ++ show n ++ " times..."
    r <- foldM go Success [1..n]
    putStrLn $ "[" ++ testName s ++ "] Result: " ++ displayResult r
    return r
  where
    go Success i = do
      (out, r) <- redirectHandle stdout $ do
        r <- runSpec pams s
        r `seq` performMajorGC
        return r
      case r of
        Success -> pure ()
        _ -> do
          putStrLn $ "[" ++ testName s ++ "] Failed on test iteration " ++ show i
          BS.putStrLn out
      return r
    go failure _ = pure failure
runSpec pams (TestSpec name (roles :: [(k, k -> StoredMVar ctx -> IO TestResult)])) = do
    putStrLn $ "[" ++ name ++ "] Running..."
    comms <- newEmptyMVar :: IO (StoredMVar ctx)
    childrenLeft <- newIORef $ length roles
    readyToGo <- StdMVar.newEmptyMVar :: IO (StdMVar.MVar ())
    let k = show $ mVarName comms
    r <- runConcurrently $ foldMap (\(l, r) -> go childrenLeft readyToGo l [name, r, k]) labelsroles
    r `seq` comms `seq` performMajorGC
    putStrLn $ "[" ++ name ++ "] Result: " ++ displayResult r
    return r
  where
    roleCount :: Map.Map String Int
    roleCount = Map.filter (> 1) $
      List.foldl' (\m (k, _) -> Map.alter (Just . maybe 1 succ) (show k) m) mempty roles

    labelsroles :: [(String, String)]
    labelsroles = snd $ List.mapAccumR (\m (k, _) -> swap $ Map.alterF (
      -- Maybe Int -> ((String, String), Maybe Int)
      maybe ((show k, show k), Nothing) (\i -> ((show k ++ "-" ++ show i, show k), Just (pred i)))
      ) (show k) m ) roleCount roles

    withLabel label s = "  [" ++ label ++ "] " ++ s

    channelHandle i handle = forever (hGetLine handle >>= (putStrLn . withLabel i))
      `catch` (const $ return mempty :: IOError -> IO ())

    mkConf x
      = setStdout createPipe
      $ setStdin createPipe
      $ setStderr byteStringOutput
      $ proc (exec pams) x

    errToResult :: String -> SomeException -> IO TestResult
    errToResult label = return . Failure . withLabel label . show

    messWithProcess label x action = withProcessTerm (mkConf x) action `catch` errToResult label

    go childrenLeft readyToGo label x = Concurrently
      $ messWithProcess label x $ \p -> hGetLine (getStdout p) >>= \handshake ->
        if handshake /= startToken
        then return (Failure $ withLabel label ("Unexpected starting token: " ++ handshake))
        else withAsyncBound (channelHandle label (getStdout p)) $ \sink -> do
          iAmLast <- atomicModifyIORef' childrenLeft (\n -> (n - 1, n == 1))
          when iAmLast $ StdMVar.putMVar readyToGo ()
          ready <- race (threadDelay $ 1000 * processStartTimeout pams) (StdMVar.readMVar readyToGo)
          case ready of
            Left () -> return $ Failure $ withLabel label "child process didn't start in time"
            Right () -> do
              hPutStrLn (getStdin p) handshake
              hClose (getStdin p)
              r <- race (threadDelay $ 1000 * timeout pams) $ do
                ecode <- (Right <$> waitExitCode p) `catch` (return . Left)
                case ecode of
                  Right ExitSuccess  -> pure Success
                  Right (ExitFailure _) -> do
                    errs <- atomically (getStderr p)
                    return $ Failure (unlines . map (withLabel label) $ lines $ unpack errs)
                  Left e -> errToResult label e
              _ <- evaluate r
              wait sink
              hFlush stdout
              hFlush stderr
              case r of
                Left () -> return $ Failure $ withLabel label $
                    "the process did not finish in " ++ show (timeout pams) ++ " milliseconds"
                Right s -> return s
