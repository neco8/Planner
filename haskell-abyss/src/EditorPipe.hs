module EditorPipe where

import           Safe           (headMay)
import           System.IO      (hClose, openTempFile)
import           System.Process (createProcess, proc, waitForProcess)


editorPipe :: String -> IO String
editorPipe before = do
  (path, handle) <- openTempFile "/tmp" "pipe.tmp"
  hClose handle
  writeFile path before
  (_, _, _, processHandle) <- createProcess $ proc "vim" [path]
  waitForProcess processHandle
  reverse . dropWhen (== '\n') . reverse <$> readFile path
    where
      dropWhen pred as =
        case pred <$> headMay as of
          Just True -> drop 1 as
          _         -> as

