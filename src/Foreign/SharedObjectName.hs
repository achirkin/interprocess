-- | Globally unique names to be used as references in the interprocess communication.
--
--   The implementation must guarantee that no two processes can generate
--   the same unique @SOName@ independently at the same time.
--   To ensure this, the implementation uses three different seeds:
--
--    1. @C@ @rand()@ with a time-dependent seed @srand(time(NULL))@
--    2. Process id taken from @getpid@ or @GetCurrentProcessId@ (unique across process).
--    3. A global auto-incremented variable (unique within a process).
--
module Foreign.SharedObjectName
  ( SOName (), hPutSOName, hGetSOName, unsafeWithSOName
  ) where


import Foreign.SharedObjectName.Internal
