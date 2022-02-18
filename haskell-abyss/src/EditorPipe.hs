module EditorPipe where

import           GHC.IO.Handle           (hDuplicate, hIsClosed, hPutStr)
import           GHC.IO.Handle.Internals (withHandle)
import           Safe                    (headMay)
import           Shh                     (exe)
import           System.IO               (IOMode (ReadWriteMode, WriteMode),
                                          getContents, hClose, hGetContents,
                                          openFile, openTempFile, stdin, stdout,
                                          withFile)
import           System.Process          (CreateProcess (new_session, std_in, std_out),
                                          StdStream (UseHandle), createProcess,
                                          proc, waitForProcess)

editorPipe :: IO ()
editorPipe = do
  (path, handle) <- openTempFile "/tmp" "pipe.tmp"
  getContents' >>= hPutStr handle
  hClose handle
  vim path
  readFile path >>= putStr . reverse . dropWhen (== '\n') . reverse
    where
      dropWhen pred as =
        case pred <$> headMay as of
          Just True -> drop 1 as
          _         -> as
      getContents' = hDuplicate stdin >>= hGetContents
      vim = exe "vim"
