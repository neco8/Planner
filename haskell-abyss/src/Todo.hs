{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Todo (makeTodoTreeValid, Todo (..), todoParser, doneAt, content, VsCodeTodo (..), IsTodo, exactTodoParser, doneAtToBool, At(..), DoneAt, toggleAt, doneAtAt, atLocalTime, TodoTree) where
import           AdditionalInformation (AdditionalInformation (..),
                                        addInformationTo,
                                        runAdditionalInformation)
import           Control.Arrow         ((&&&))
import           Data.Fixed            (Fixed (MkFixed))
import           Data.Function         (fix, on)
import           Data.Functor          ((<$))
import           Data.Functor.Base     (TreeF (NodeF))
import           Data.Functor.Foldable (Recursive (cata))
import           Data.Functor.Identity (Identity (..))
import           Data.List             (sort, sortOn)
import           Data.List.NonEmpty    (nonEmpty)
import           Data.Ratio            ((%))
import           Data.Text             (Text, pack, unpack)
import           Data.Time             (LocalTime (LocalTime), TimeZone,
                                        UTCTime,
                                        ZonedTime (ZonedTime, zonedTimeToLocalTime, zonedTimeZone),
                                        addUTCTime, defaultTimeLocale,
                                        formatTime, fromGregorianValid,
                                        getCurrentTime, getCurrentTimeZone,
                                        getZonedTime, makeTimeOfDayValid,
                                        utcToZonedTime, zonedTimeToUTC)
import qualified Data.Tree             as Tree (Tree (..), rootLabel, subForest)
import           Data.Vector           (Vector)
import           GHC.Base              (coerce)
import           Lens.Micro            (Lens', LensLike', Traversal', _Just,
                                        each, filtered, lens, mapped, to, (%~),
                                        (&), (.~), (^.), (^..), (^?))
import           Lens.Micro.TH         (makeLenses)
import           PPrint                (PPrint, pprint)
import           Parser                (Parser, parserFromMaybe)
import           Safe.Foldable         (maximumMay)
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

doneAtAt :: Traversal' DoneAt At
doneAtAt afb s = case s of
  Done aAt -> Done <$> afb aAt
  UnDone   -> pure UnDone

atLocalTime :: Lens' At LocalTime
atLocalTime = lens
  (\(At zoned) ->
    zonedTimeToLocalTime zoned
  )
  (\(At zoned) localB ->
    At $ ZonedTime localB $ zonedTimeZone zoned
  )

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
  { _todoDoneAt :: DoneAt
  , _content    :: s
  } deriving (Eq, Ord, Show, Functor)
makeLenses ''Todo

class IsTodo t where
  doneAt :: Lens' t DoneAt

instance IsTodo (Todo s) where
  doneAt = todoDoneAt

newtype TodoTree s = TodoTree (Tree.Tree (Todo s))

runTodoTree :: TodoTree s -> Tree.Tree (Todo s)
runTodoTree = coerce

instance PPrint s => PPrint (TodoTree s) where
  pprint = pprint . runTodoTree

instance Eq s => Eq (TodoTree s) where
  (==) = (==) `on` runTodoTree

instance Ord s => Ord (TodoTree s) where
  compare = compare `on` (^. to runTodoTree . rootLabel . to ((^. doneAt) &&& (^. content)))

rootLabel :: Lens' (Tree.Tree a) a
rootLabel = lens Tree.rootLabel (\tree label ->
  Tree.Node label $ Tree.subForest tree
  )

subForest :: Lens' (Tree.Tree a) [Tree.Tree a]
subForest = lens Tree.subForest (Tree.Node . Tree.rootLabel)

makeTodoTreeValid :: Ord s => (Tree.Tree (Todo s) -> Tree.Tree (Todo s)) -> Tree.Tree (Todo s) -> TodoTree s
makeTodoTreeValid makeTodoTreeSValid tree =
  TodoTree . sortTodoTree . makeTodoTreeSValid $ case (isDone &&& latestDoneAt) tree of
    (True, Just d) -> ((rootLabel . doneAt) .~ d) tree
    _              -> ((rootLabel . doneAt) .~ UnDone) tree
    where
      isDone :: Tree.Tree (Todo s) -> Bool
      isDone tree = case tree ^. subForest . to nonEmpty . to (fmap $ all isDone) of
        Just bool -> bool
        Nothing   -> tree ^. rootLabel . doneAt . to doneAtToBool
      latestDoneAt :: Tree.Tree (Todo s) -> Maybe DoneAt
      latestDoneAt tree =
        cata (\case
          NodeF todo [] -> todo ^. doneAt . to (toMaybe doneAtToBool)
          NodeF todo mlatests -> maximumMay (((todo ^. doneAt . to (toMaybe doneAtToBool)) : mlatests) ^.. each . _Just))
          tree
      sortTodoTree :: Ord s => Tree.Tree (Todo s) -> Tree.Tree (Todo s)
      sortTodoTree tree =
        tree
          & subForest %~ sortOn (^. rootLabel . content)
          & subForest %~ sortOn (^. rootLabel . doneAt . to doneAtToBool)
          & subForest . mapped %~ sortTodoTree

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe pred a =
  if pred a then
    Just a
  else
    Nothing

instance PPrint s => PPrint (Todo s) where
  pprint =
    showDoneWith $ \b -> "- [" <> (if b then "x" else " ") <> "] "

newtype VsCodeTodo s = VsCodeTodo (Todo s) deriving (Eq, Ord, Show)

instance IsTodo (VsCodeTodo s) where
  doneAt = lens (\(VsCodeTodo vsCodeTodo) -> vsCodeTodo ^. doneAt) $ \(VsCodeTodo vsCodeTodo) b ->
    VsCodeTodo $ doneAt .~ b $ vsCodeTodo

instance PPrint s => PPrint (VsCodeTodo s) where
  pprint (VsCodeTodo todo) =
    showDoneWith (\b -> if b then "✔ " else "☐ ") todo

showDoneWith :: PPrint s => (Bool -> Text) -> Todo s -> Text
showDoneWith f (Todo d s) =
  f (doneAtToBool d) <> pprint s <> pprint (AdditionalInformation $ pprint d)

instance (IsTodo a, IsTodo b) => IsTodo (Either a b) where
  doneAt = lens (\case
    Right a -> a ^. doneAt
    Left b  -> b ^. doneAt
    ) $ \e bool -> case e of
    Right a -> Right $ (doneAt .~ bool) a
    Left b  -> Left $ (doneAt .~ bool) b

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

-- util

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
