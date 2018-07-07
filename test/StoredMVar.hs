module Main where

-- import           Control.Concurrent.Process.StoredMVar
-- import           Control.Exception                     (SomeException, catch,
--                                                         displayException)
-- import           Foreign.SharedObjectName
-- import           GHC.Environment                       (getFullArgs)
-- import           System.Environment
import           System.Exit



-- | A number of processes trying to do something concurrently.
--   For example, read from the same StoredMVar.
--   Actual number of processes may vary depending on a test case.
--   Minimum allowed number is 1.
concProcBaseN :: Int
concProcBaseN = 2

data BasicRole = Master | Slave
  deriving (Eq, Ord, Show, Read)

data TestSpec
  = SimpleTakePut BasicRole
  deriving (Eq, Ord, Show, Read)


main :: IO ()
main = exitSuccess
