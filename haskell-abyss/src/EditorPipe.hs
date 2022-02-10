module EditorPipe where

import           GHC.IO.Handle (hDuplicate)
import           Safe          (headMay)
import           Shh           (exe)
import           System.IO     (hClose, hGetContents, openTempFile, stdin)

editorPipe :: IO String
editorPipe = do
  (path, handle) <- openTempFile "/tmp" "pipe.tmp"
  hClose handle
  getContents' >>= writeFile path
  vim path
  reverse . dropWhen (== '\n') . reverse <$> readFile path
    where
      dropWhen pred as =
        case pred <$> headMay as of
          Just True -> drop 1 as
          _         -> as
      getContents' = hDuplicate stdin >>= hGetContents
      vim = exe "vim"
