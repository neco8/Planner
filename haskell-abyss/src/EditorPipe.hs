module EditorPipe where

import           Data.Maybe     (fromMaybe)
import           Safe           (headMay)
import           System.IO      (hClose, openTempFile)
import           System.Process (createProcess, proc, waitForProcess)


editorPipe :: Maybe String -> String -> IO String
editorPipe meditor before = do
  (path, handle) <- openTempFile "/tmp" "pipe.tmp"
  hClose handle
  writeFile path before
  let editor = fromMaybe "vim" meditor
  (_, _, _, processHandle) <- createProcess $ proc editor [path]
  waitForProcess processHandle
  reverse . dropWhen (== '\n') . reverse <$> readFile path
    where
      dropWhen pred as =
        case pred <$> headMay as of
          Just True -> drop 1 as
          _         -> as

