{-# LANGUAGE DataKinds                 #-}
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
import           Lens.Micro             (to, (%~), (^.))
import           Options.Declarative
import           PPrint                 (PPrint (pprint))
import           Parser                 (treeParser)
import           QuickWinAnalysis       (QuickWinAnalysis, qwaParser)
import           Text.Megaparsec        (errorBundlePretty, optional, parse,
                                         some)
import           Text.Megaparsec.Char   (newline)
import           Todo                   (IsTodo, Todo, VsCodeTodo (..), isDone,
                                         todoParser)

main :: IO ()
main = run "Planner" (Just "0.0.0") compile

-- TODO: todo編集用のtui。設定ファイルがあって、それには入力用・保存用のファイルが書いてある。コマンドでも指定可能
-- TODO: チャート用のfilepath

compile :: Flag "i" '["input"] "FILE_PATH" "input file path" (Maybe FilePath)
  -> Flag "o" '["output"] "FILE_PATH" "output file path" (Maybe FilePath)
  -> Flag "" '["vscode"] "" "vscode todo style" Bool
  -> Cmd "compile command" ()
compile iPath oPath isVsCode = do
  let input = maybe (fmap pack getContents) (fmap pack . readFile) (get iPath)
      output = maybe (putStrLn . unpack) ((. unpack) . writeFile) (get oPath)
      wrapTodo | get isVsCode = Right . VsCodeTodo
               | otherwise = Left
  i <- liftIO input
  liftIO $ case parse (some $ apmParser (treeParser (wrapTodo <$> todoParser qwaParser)) <* optional newline) "" i of
    Left err   -> putStrLn $ errorBundlePretty err
    Right apms -> output . pprint $ sortAPM <$> L.sort apms


sortAPM :: (Ord t, IsTodo t) => ActionPriorityMatrix (Tree t) -> ActionPriorityMatrix (Tree t)
sortAPM =
  qwas %~ uncurry (<>) . partition (not . all (^. to isDone)) . sort

sort :: Ord a => Vector a -> Vector a
sort = fromList . L.sort . toList
