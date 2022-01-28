{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Todo (Todo (..), todoParser, isDone, content, VsCodeTodo (..), IsTodo, exactTodoParser, doneAtToBool, At(..), DoneAt, toggleAt) where
import           AdditionalInformation (AdditionalInformation (..),
                                        addInformationTo,
                                        runAdditionalInformation)
import           Data.Fixed            (Fixed (MkFixed))
import           Data.Function         (fix, on)
import           Data.Functor          ((<$))
import           Data.Ratio            ((%))
import           Data.Text             (Text, pack, unpack)
import           Data.Time             (LocalTime (LocalTime), TimeZone,
                                        UTCTime,
                                        ZonedTime (ZonedTime, zonedTimeZone),
                                        defaultTimeLocale, formatTime,
                                        fromGregorianValid, getCurrentTime,
                                        getCurrentTimeZone, makeTimeOfDayValid,
                                        zonedTimeToUTC)
import           Data.Vector           (Vector)
import           GHC.Base              (coerce)
import           Lens.Micro            (Lens', lens, (.~), (^.))
import           Lens.Micro.TH         (makeLenses)
import           PPrint                (PPrint, pprint)
import           Parser                (Parser, parserFromMaybe)
import           Text.Megaparsec       (choice, count, count', parseMaybe,
                                        parseTest, try, (<|>))
import           Text.Megaparsec.Char  (char, digitChar, string)
import           Text.Read             (readMaybe)

newtype At = At ZonedTime deriving (Show)

runAt :: At -> ZonedTime
runAt = coerce

instance Eq At where
  (==) = (==) `on` (zonedTimeToUTC . runAt)

instance Ord At where
  compare = compare `on` (zonedTimeToUTC . runAt)

instance PPrint At where
  pprint (At time) = pack $ formatTime defaultTimeLocale format time
    where
      format = "%Y/%m/%d-%H:%M:%S"

data DoneAt = Done At | UnDone deriving (Eq, Ord, Show)

instance PPrint DoneAt where
  pprint (Done at) = pprint at
  pprint UnDone    = ""

doneAtToBool :: DoneAt -> Bool
doneAtToBool (Done _) = True
doneAtToBool UnDone   = False

toggleAt :: ZonedTime -> DoneAt -> DoneAt
toggleAt time (Done _) = UnDone
toggleAt time UnDone   = Done $ At time

data Todo s = Todo
  { _isTodoDone :: DoneAt
  , _content    :: s
  } deriving (Eq, Ord, Show, Functor)
makeLenses ''Todo

class IsTodo t where
  isDone :: Lens' t DoneAt

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
showDoneWith f (Todo d s) =
  f (doneAtToBool d) <> pprint s <> pprint (AdditionalInformation $ pprint d)

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
  pure (At <$> mzonedTime)
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

todoWithoutTimeParser_ :: Vector (Parser Bool) -> Parser s -> Parser (At -> Todo s)
todoWithoutTimeParser_ pdones ps = do
  b <- try (choice pdones) <|> pure False
  s <- ps
  pure $ \at ->
    if b then
      Todo (Done at) s
    else
      Todo UnDone s

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

todoWithoutTimeParser :: Parser s -> Parser (At -> Todo s)
todoWithoutTimeParser = todoWithoutTimeParser_ boolParsers

todoParser :: ZonedTime -> Parser s -> Parser (Todo s)
todoParser now p = do
  (getTodo, information) <- addInformationTo (todoWithoutTimeParser p)
  case rightMost (parseMaybe (atParser (zonedTimeZone now))) (runAdditionalInformation <$> information) of
    Just at -> pure . getTodo $ at
    _       -> pure . getTodo $ At now

exactTodoWithoutTimeParser :: Parser s -> Parser (At -> Todo s)
exactTodoWithoutTimeParser ps = do
  b <- choice boolParsers
  s <- ps
  pure $ \at ->
    if b then
      Todo (Done at) s
    else
      Todo UnDone s

exactTodoParser :: ZonedTime -> Parser s -> Parser (Todo s)
exactTodoParser now p = do
  (getTodo, information) <- addInformationTo (exactTodoWithoutTimeParser p)
  case rightMost (parseMaybe (atParser (zonedTimeZone now))) (runAdditionalInformation <$> information) of
    Just at -> pure . getTodo $ at
    _       -> pure . getTodo $ At now

rightMost :: (a -> Maybe b) -> [a] -> Maybe b
rightMost f prevls = do
  lasta <- last' prevls
  inita <- init' prevls
  case f lasta of
    Just b  -> Just b
    Nothing -> rightMost f inita

last' :: [a] -> Maybe a
last' ls = case length ls of
  0 -> Nothing
  _ -> Just $ last ls

init' :: [a] -> Maybe [a]
init' ls = case length ls of
  0 -> Nothing
  _ -> Just $ init ls
