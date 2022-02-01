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
import           Data.Maybe             (fromMaybe, isJust, maybe)
import           Data.Text              (Text, lines, pack)
import           Data.Text.IO           (getContents, getLine, putStr, putStrLn,
                                         readFile, writeFile)
import           Data.Time              (addLocalTime, getZonedTime,
                                         zonedTimeToUTC)
import           Data.Tree              (Tree (Node, rootLabel, subForest))
import           Data.Vector            (Vector, fromList, partition, toList)
import           ForWork                (ForWorkActionPriorityMatrix (..),
                                         changeFilePathForWork)
import           Lens.Micro             (_2, mapped, sets, to, (%~), (<%~),
                                         (^.))
import           Options.Declarative    (Arg, Cmd, Flag, Group (..),
                                         Option (get), run, subCmd)
import           PPrint                 (PPrint (pprint))
import           Parser                 (treeParser)
import           Prelude                hiding (getContents, getLine, lines,
                                         putStr, putStrLn, readFile, writeFile)
import           QuickWinAnalysis       (QuickWinAnalysis, qwaParser)
import           Replace.Megaparsec     (streamEdit)
import           System.Exit            (die)
import           Text.Megaparsec        (errorBundlePretty, optional, parse,
                                         some)
import           Text.Megaparsec.Char   (newline)
import           Todo                   (IsTodo, Todo, VsCodeTodo (..),
                                         atLocalTime, doneAt, doneAtAt,
                                         doneAtToBool, exactTodoParser,
                                         todoParser, toggleAt)

logo = "██████╗ ██╗      █████╗ ███╗   ██╗███╗   ██╗███████╗██████╗ \n██╔══██╗██║     ██╔══██╗████╗  ██║████╗  ██║██╔════╝██╔══██╗ \n██████╔╝██║     ███████║██╔██╗ ██║██╔██╗ ██║█████╗  ██████╔╝ \n██╔═══╝ ██║     ██╔══██║██║╚██╗██║██║╚██╗██║██╔══╝  ██╔══██╗ \n██║     ███████╗██║  ██║██║ ╚████║██║ ╚████║███████╗██║  ██║ \n╚═╝     ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝ \n"

main :: IO ()
main = run "Planner" (Just "0.2.0") $
  Group ("Planner program" <> "\n\n" <> logo)
    [ subCmd "compile"  compile
    , subCmd "function" function
    ]

-- TODO: todo編集用のtui。設定ファイルがあって、それには入力用・保存用のファイルが書いてある。コマンドでも指定可能

data Argument = Argument
  { _input             :: Maybe InputPath,
    _output            :: Maybe OutputPath,
    _inputOutput       :: Maybe InputOutputPath,
    _isVsCodeTodoStyle :: IsVsCodeTodoStyle,
    _chartPath         :: Maybe ChartPath,
    _forWorkSuffix     :: Maybe ForWorkSuffix
  }

newtype InputPath = InputPath FilePath

newtype OutputPath = OutputPath FilePath

newtype InputOutputPath = InputOutputPath FilePath

newtype IsVsCodeTodoStyle = IsVsCodeTodoStyle Bool

newtype ChartPath = ChartPath FilePath

newtype ForWorkSuffix = ForWorkSuffix Text

compile ::
  Flag "i" '["input"] "FILE_PATH" "input file path" (Maybe FilePath) ->
  Flag "o" '["output"] "FILE_PATH" "output file path" (Maybe FilePath) ->
  Flag "" '["vscode"] "" "vscode todo style" Bool ->
  Flag "" '["chart"] "FILE_PATH" "chart file path" (Maybe FilePath) ->
  Flag "" '["work-suffix"] "FOR_WORK_FILE_SUFFIX" "for work generation suffix" (Maybe String) ->
  Flag "f" '["input-output"] "FILE_PATH" "input and output file path" (Maybe FilePath) ->
  Cmd "compile command" ()
compile iPath oPath isVsCode chartPath forWorkS ioPath =
  liftIO $ compile' $
    Argument
      (InputPath <$> get iPath)
      (OutputPath <$> get oPath)
      (InputOutputPath <$> get ioPath)
      (IsVsCodeTodoStyle $ get isVsCode)
      (ChartPath <$> get chartPath)
      (ForWorkSuffix . pack <$> get forWorkS)

compile' :: Argument -> IO ()
compile' Argument {..} = do
  let input = maybe getContents readFile (coerce _inputOutput <|> coerce _input)
      output = maybe putStrLn writeFile (coerce _inputOutput <|> coerce _output)
      outputForWork apms = fromMaybe (pure ()) $ do
        path <- coerce _inputOutput <|> coerce _output
        forWorkPath <- changeFilePathForWork (coerce _forWorkSuffix) path
        pure $ writeFile forWorkPath . pprint $ ForWorkAPM <$> apms
      chart = maybe (const $ pure ()) getChart (coerce _chartPath)
      wrapTodo
        | coerce _isVsCodeTodoStyle = Right . VsCodeTodo
        | otherwise = Left
  parse_ input $ \apms -> do
    output . pprint $ MDAPM . fmap (fmap wrapTodo) <$> apms
    outputForWork apms
    chart apms

parse_ :: IO Text -> ([ActionPriorityMatrix (Tree (Todo QuickWinAnalysis))] -> IO ()) -> IO ()
parse_ input f = do
  i <- input
  zonedTime <- getZonedTime
  case parse (some $ apmParser (treeParser (todoParser zonedTime qwaParser)) <* optional newline) "" i of
    Left err -> die $ errorBundlePretty err
    Right as -> do
      let apms = (qwas %~ sortTodoQWA) <$> L.sort as
      f apms

sortTodoQWA :: (Ord t, IsTodo t) => Vector (Tree t) -> Vector (Tree t)
sortTodoQWA =
  uncurry (<>) . partition (not . all (^. doneAt . to doneAtToBool)) . sort . (mapped . sets (\f s -> Node (rootLabel s) $ f $ subForest s) %~ (toList . sortTodoQWA . fromList))

sort :: Ord a => Vector a -> Vector a
sort = fromList . L.sort . toList

function :: Flag "t" '["toggle-done"] "" "toggle done todo" Bool ->
  Flag "" '["adjust-done-day"] "HOW_MANY_DAYS_TO_ADJUST" "adjust done-day back and forth" (Maybe Int) ->
  Cmd "function command" ()
function isToggle howManyDays = liftIO $ function' (get isToggle) (get howManyDays)

function' :: Bool -> Maybe Int -> IO ()
function' isToggle mhowManyDays = do
  zonedTime <- getZonedTime
  case () of
    _
      | isToggle -> io $ toggleDone zonedTime
      | isJust mhowManyDays -> io $ adjustTime zonedTime mhowManyDays
      | otherwise -> parse_ getContents (putStrLn . pprint . fmap MDAPM)
  where
    io f = do
      inputs <- lines <$> getContents
      sequence_ $ putStrLn . f <$> inputs
    toggleDone zonedTime = streamEdit (exactTodoParser zonedTime qwaParser) $
      pprint . (doneAt %~ toggleAt zonedTime)
    adjustTime _ Nothing = id
    adjustTime zonedTime (Just howManyDays) = streamEdit (exactTodoParser zonedTime qwaParser) $
      pprint . (doneAt . doneAtAt . atLocalTime %~ addLocalTime (fromInteger $ toInteger $ howManyDays * 24 * 60 * 60))
