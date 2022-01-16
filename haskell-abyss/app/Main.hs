{-# LANGUAGE DataKinds #-}
module Main where

import           ActionPriorityMatrix   (ActionPriorityMatrix, apmParser, qwas)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (all)
import           Data.Functor           (void)
import qualified Data.List              as L (sort)
import           Data.Maybe             (maybe)
import           Data.Text              (Text, pack, unpack)
import           Data.Tree
import           Data.Vector            (Vector, fromList, partition, toList)
import           Lens.Micro
import           Options.Declarative
import           PPrint                 (PPrint (pprint))
import           Parser                 (treeParser)
import           QuickWinAnalysis       (QuickWinAnalysis, qwaParser)
import           Text.Megaparsec        (errorBundlePretty, optional, parse,
                                         some)
import           Text.Megaparsec.Char   (newline)
import           Todo                   (Todo, VsCodeTodo (..), isDone,
                                         todoParser)

main :: IO ()
main = run "Planner" (Just "0.0.0") compile

-- TODO: todo編集用のtui。設定ファイルがあって、それには入力用・保存用のファイルが書いてある。コマンドでも指定可能
-- TODO: チャート用のfilepath

compile :: Flag "i" '["input"] "FILE_PATH" "input file path" (Maybe FilePath)
  -> Flag "o" '["output"] "FILE_PATH" "output file path" (Maybe FilePath)
  -> Flag "" '["vscode"] "" "vscode todo style" Bool
  -> Cmd "compile command" ()
compile iPath oPath isVscode = do
  let printTodo :: Todo QuickWinAnalysis -> Text
      printTodo
        | get isVscode = pprint . VsCodeTodo
        | otherwise = pprint
      input = maybe (fmap pack getContents) (fmap pack . readFile) (get iPath)
      output = maybe (putStrLn . unpack) ((. unpack) . writeFile) (get oPath)
  i <- liftIO input
  liftIO $ case parse (some $ apmParser (treeParser (todoParser qwaParser)) <* optional newline) "" i of
    Left err   -> putStrLn $ errorBundlePretty err
    Right apms -> output . pprint $ sortAPM <$> L.sort apms


sortAPM :: ActionPriorityMatrix (Tree (Todo QuickWinAnalysis)) -> ActionPriorityMatrix (Tree (Todo QuickWinAnalysis))
sortAPM =
  qwas %~ uncurry (<>) . partition (not . all (^. isDone)) . sort

sort :: Ord a => Vector a -> Vector a
sort = fromList . L.sort . toList
