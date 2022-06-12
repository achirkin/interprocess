{-# LANGUAGE LambdaCase #-}
module Tools.Handle
    ( redirectHandle
    ) where

import Control.Exception   (bracket)
import GHC.IO.Handle       (hDuplicate, hDuplicateTo, hFlushAll)
import GHC.IO.Handle.Types (Handle (DuplexHandle, FileHandle))
import System.IO
import System.IO.Temp      (withSystemTempFile)


-- | Temporary redirect the output of a handle to an output String (e.g. `stdout` or `stderr`).
redirectHandle :: Handle -> IO a -> IO (String, a)
redirectHandle origHandle action = withSystemTempFile "interprocess.out" $ \_ tempFile -> do
    start <- hGetPosn tempFile
    hGetEncoding origHandle >>= \case
      Just e  -> hSetEncoding tempFile e
      Nothing -> hSetBinaryMode tempFile True
    hSetBuffering tempFile NoBuffering
    hSetNewlineMode tempFile noNewlineTranslation
    r <- bracket (redirect $ hGetWriteEnd tempFile) unredirect (const action)
    hFlushAll tempFile
    r `seq` hSetPosn start
    out <- hGetContents tempFile
    force out
    return (out, r)
  where
    redirect tempFile = do
      origDup <- hDuplicate origHandleOut
      hDuplicateTo tempFile origHandleOut
      return origDup

    unredirect origDup = hDuplicateTo origDup origHandleOut >> hClose origDup

    origHandleOut = hGetWriteEnd origHandle

    force []     = pure ()
    force (_:cs) = force cs

    hGetWriteEnd (DuplexHandle p _ w) = FileHandle p w
    hGetWriteEnd h                    = h
