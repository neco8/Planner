{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Todo (Todo (..), todoParser, isDone, content, VsCodeTodo (..), IsTodo, exactTodoParser) where
import           Data.Functor         ((<$))
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import           GHC.Base             (coerce)
import           Lens.Micro           (Lens', lens, (.~), (^.))
import           Lens.Micro.TH        (makeLenses)
import           PPrint               (PPrint, pprint)
import           Parser               (Parser)
import           Text.Megaparsec      (choice, try, (<|>))
import           Text.Megaparsec.Char (char, string)

data Todo s = Todo
  { _isTodoDone :: Bool
  , _content    :: s
  } deriving (Eq, Ord, Show, Functor)
makeLenses ''Todo

class IsTodo t where
  isDone :: Lens' t Bool

instance IsTodo (Todo s) where
  isDone = isTodoDone

instance PPrint s => PPrint (Todo s) where
  pprint =
    showDoneWith $ \b -> "- [" <> (if b then "x" else " ") <> "] "

newtype VsCodeTodo s = VsCodeTodo (Todo s) deriving (Eq, Ord, Show)

instance IsTodo (VsCodeTodo s) where
  isDone = lens (\(VsCodeTodo vsCodeTodo) -> vsCodeTodo ^. isDone) $ \(VsCodeTodo vsCodeTodo) b ->
    VsCodeTodo $ isDone .~ b $ vsCodeTodo

instance PPrint s => PPrint (VsCodeTodo s) where
  pprint (VsCodeTodo todo) =
    showDoneWith (\b -> if b then "✔ " else "☐ ") todo

showDoneWith :: PPrint s => (Bool -> Text) -> Todo s -> Text
showDoneWith f (Todo b s) =
  f b <> pprint s

instance (IsTodo a, IsTodo b) => IsTodo (Either a b) where
  isDone = lens (\case
    Right a -> a ^. isDone
    Left b  -> b ^. isDone
    ) $ \e bool -> case e of
    Right a -> Right $ (isDone .~ bool) a
    Left b  -> Left $ (isDone .~ bool) b

-- parser

atParser :: TimeZone -> Parser At
atParser timeZone = parserFromMaybe "failed with constructing At." $ do
  year <- rangeParser 2000 3000
  char '/'
  month <- rangeParser 1 12
  char '/'
  day <- rangeParser 1 31
  char '-'
  hour <- rangeParser 0 23
  char ':'
  minute <- rangeParser 0 59
  char ':'
  second <- rangeParser 0 59
  let mtod = makeTimeOfDayValid hour minute . fromRational . (% 1) $ toInteger second
      mdoy = fromGregorianValid (toInteger year) month day
      mzonedTime = flip ZonedTime timeZone <$> (LocalTime <$> mdoy <*> mtod)
      mutcTime = zonedTimeToUTC <$> mzonedTime
  pure (At <$> mutcTime)
    where
      rangeParser :: Int -> Int -> Parser Int
      rangeParser min max = do
        let minNumberOfDigit = getNumberOfDigit min
            maxNumberOfDigit = getNumberOfDigit max
        numStr <- count' minNumberOfDigit maxNumberOfDigit digitChar
        case readMaybe numStr :: Maybe Int of
          Just num -> if num >= min && num <= max then
            pure num
          else
            fail $ "invalid range. " <> show min <> " <= x <= " <> show max <> "."
          Nothing -> fail $ "this is not a number. '" <> numStr <> "'"
      getNumberOfDigit :: Int -> Int
      getNumberOfDigit num =
        let f :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> (Int, Int)
            f r (nod, n)
              | n < 10 = (nod, n)
              | otherwise = r (nod + 1, n `div` 10)
         in fst $ fix f (1, num)

todoParser_ :: Vector (Parser Bool) -> Parser s -> Parser (Todo s)
todoParser_ pdones ps = do
  b <- try (choice pdones) <|> pure False
  Todo b <$> ps

doneMDParser :: Parser Bool
doneMDParser = do
  string "- ["
  b <- (True <$ char 'x') <|> (False <$ char ' ')
  string "] "
  pure b

doneVsCodeParser :: Parser Bool
doneVsCodeParser = try (True <$ string "✔ ") <|> (False <$ string "☐ ")

boolParsers :: Vector (Parser Bool)
boolParsers = [doneMDParser, doneVsCodeParser]

todoParser :: Parser s -> Parser (Todo s)
todoParser = todoParser_ boolParsers

exactTodoParser :: Parser s -> Parser (Todo s)
exactTodoParser ps = do
  b <- choice boolParsers
  Todo b <$> ps
