{-# LANGUAGE LambdaCase #-}
module Tools.Handle (redirectHandle) where

import Control.Exception     (bracket)
import Data.ByteString.Char8 (ByteString, hGetContents)
import GHC.IO.Handle         (hDuplicate, hDuplicateTo, hFlushAll)
import GHC.IO.Handle.Types   (Handle (DuplexHandle, FileHandle))
import System.IO             hiding (hGetContents)
import System.IO.Temp        (withSystemTempFile)

-- | Temporary redirect the output of a handle to an output ByteString (e.g. `stdout` or `stderr`).
redirectHandle :: Handle -> IO a -> IO (ByteString, a)
redirectHandle origHandle action = withSystemTempFile "interprocess.out" $ \_ tempFile -> do
    start <- hGetPosn tempFile
    hGetEncoding origHandle >>= \case
      Just e  -> hSetEncoding tempFile e
      Nothing -> hSetBinaryMode tempFile True
    hSetBuffering tempFile NoBuffering
    hSetNewlineMode tempFile noNewlineTranslation
    r <- bracket (redirect $ hGetWriteEnd tempFile) unredirect (const action)
    r `seq` hFlushAll tempFile
    hSetPosn start
    out <- hGetContents tempFile
    return (out, r)
  where
    redirect tempFile = do
      origDup <- hDuplicate origHandleOut
      hDuplicateTo tempFile origHandleOut
      return origDup

    unredirect origDup = hDuplicateTo origDup origHandleOut >> hClose origDup

    origHandleOut = hGetWriteEnd origHandle

    hGetWriteEnd (DuplexHandle p _ w) = FileHandle p w
    hGetWriteEnd h                    = h
