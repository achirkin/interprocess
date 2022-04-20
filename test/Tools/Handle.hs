module Tools.Handle
    ( redirectHandle
    ) where

import Control.Exception       (bracket)
import GHC.IO.Handle           (hDuplicate, hDuplicateTo)
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Types     ( Handle(FileHandle, DuplexHandle), Handle__(haInputNL, haOutputNL) )
import System.IO
import System.Process          (createPipe)


-- | Temporary redirect the output of a handle to a ByteString (e.g. `stdout` or `stderr`).
redirectHandle :: Handle -> IO a -> IO (String, a)
redirectHandle origHandle action = usingReadEnd origHandle $ \readEnd terminateOutput -> do
    s <- hGetContents readEnd
    r <- action
    terminateOutput
    force s
    return (s, r)
  where
    force []     = pure ()
    force (_:cs) = force cs

usingReadEnd :: Handle -> (Handle -> IO () -> IO a) -> IO a
usingReadEnd origHandle action = withCompatiblePipe origHandle $ \(readEnd, writeEnd) ->
    bracket (redirect writeEnd) unredirect (const $ action readEnd (hClose origHandle >> hClose writeEnd))
  where
    redirect writeEnd = do
      origDup <- hDuplicate origHandle
      hDuplicateWriteEndTo writeEnd origHandle
      return origDup

    unredirect origDup = hDuplicateTo origDup origHandle

hDuplicateWriteEndTo :: Handle -> Handle -> IO ()
hDuplicateWriteEndTo h1@FileHandle{} (DuplexHandle p _ w) = hDuplicateTo h1 (FileHandle p w)
hDuplicateWriteEndTo (DuplexHandle p _ w) h2@FileHandle{} = hDuplicateTo (FileHandle p w) h2
hDuplicateWriteEndTo h1 h2 = hDuplicateTo h1 h2

withCompatiblePipe :: Handle -> ((Handle, Handle) -> IO a) -> IO a
withCompatiblePipe origHandle action = do
    hFlush origHandle
    encoding <- hGetEncoding origHandle
    newline <- hGetNewlineMode origHandle
    buffering <- hGetBuffering origHandle

    let applyModes h = do
          case encoding of
            Just e  -> hSetEncoding h e
            Nothing -> hSetBinaryMode h True
          hSetNewlineMode h newline
          hSetBuffering h buffering

        closeTempHandles (readEnd, writeEnd)
          = hClose writeEnd >> hClose readEnd

        useTempHandles hs@(readEnd, writeEnd)
          = applyModes readEnd >> applyModes writeEnd >> action hs

    bracket createPipe closeTempHandles useTempHandles

hGetNewlineMode :: Handle -> IO NewlineMode
hGetNewlineMode hdl =
  withHandle_ "hGetNewlineMode" hdl $ \h_ ->
    return $ NewlineMode
      { inputNL = haInputNL h_
      , outputNL = haOutputNL h_
      }
