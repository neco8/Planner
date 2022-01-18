{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           ActionPriorityMatrix   (ActionPriorityMatrix,
                                         MDActionPriorityMatrix (..), apmParser,
                                         qwas)
import           Chart                  (getChart)
import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Coerce            (coerce)
import           Data.Foldable          (all)
import           Data.Functor           (void)
import qualified Data.List              as L (sort)
import           Data.Maybe             (fromMaybe, maybe)
import           Data.Text              (Text, pack, unpack)
import           Data.Tree              (Tree (Node, rootLabel, subForest))
import           Data.Vector            (Vector, fromList, partition, toList)
import           ForWork                (ForWorkActionPriorityMatrix (..),
                                         changeFilePathForWork)
import           Lens.Micro             (mapped, sets, to, (%~), (^.))
import           Options.Declarative    (Arg, Cmd, Flag, Option (get), run)
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

data Argument = Argument
  { _input             :: Maybe InputPath,
    _output            :: Maybe OutputPath,
    _inputOutput       :: Maybe InputOutputPath,
    _isVsCodeTodoStyle :: IsVsCodeTodoStyle,
    _chartPath         :: Maybe ChartPath
  }

newtype InputPath = InputPath FilePath

newtype OutputPath = OutputPath FilePath

newtype InputOutputPath = InputOutputPath FilePath

newtype IsVsCodeTodoStyle = IsVsCodeTodoStyle Bool

newtype ChartPath = ChartPath FilePath

compile ::
  Flag "i" '["input"] "FILE_PATH" "input file path" (Maybe FilePath) ->
  Flag "o" '["output"] "FILE_PATH" "output file path" (Maybe FilePath) ->
  Flag "" '["vscode"] "" "vscode todo style" Bool ->
  Flag "" '["chart"] "FILE_PATH" "chart file path" (Maybe FilePath) ->
  Arg "input-output" (Maybe FilePath) ->
  Cmd "compile command" ()
compile iPath oPath isVsCode chartPath ioPath = liftIO $ compile' $ Argument (InputPath <$> get iPath) (OutputPath <$> get oPath) (InputOutputPath <$> get ioPath) (IsVsCodeTodoStyle $ get isVsCode) (ChartPath <$> get chartPath)

compile' :: Argument -> IO ()
compile' Argument {..} = do
  let input = maybe (fmap pack getContents) (fmap pack . readFile) (coerce _inputOutput <|> coerce _input)
      output = maybe (putStrLn . unpack) ((. unpack) . writeFile) (coerce _inputOutput <|> coerce _output)
      outputForWork apms = fromMaybe (pure ()) $ do
        path <- coerce _inputOutput <|> coerce _output
        forWorkPath <- changeFilePathForWork path
        pure $ writeFile forWorkPath . unpack . pprint $ ForWorkAPM <$> apms
      chart = maybe (const $ pure ()) getChart (coerce _chartPath)
      wrapTodo
        | coerce _isVsCodeTodoStyle = Right . VsCodeTodo
        | otherwise = Left
  i <- input
  case parse (some $ apmParser (treeParser (todoParser qwaParser)) <* optional newline) "" i of
    Left err -> putStrLn $ errorBundlePretty err
    Right as -> do
      let apms = (qwas %~ sortTodoQWA) <$> L.sort as
      output . pprint $ MDAPM . fmap (fmap wrapTodo) <$> apms
      outputForWork apms
      chart apms

sortTodoQWA :: (Ord t, IsTodo t) => Vector (Tree t) -> Vector (Tree t)
sortTodoQWA =
  uncurry (<>) . partition (not . all (^. to isDone)) . sort . (mapped . sets (\f s -> Node (rootLabel s) $ f $ subForest s) %~ (toList . sortTodoQWA . fromList))

sort :: Ord a => Vector a -> Vector a
sort = fromList . L.sort . toList
