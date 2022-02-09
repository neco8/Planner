{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module QuickWinAnalysis where
import           Data.Coerce                (coerce)
import           Data.Function              (on)
import           Data.Functor.Base          (TreeF (NodeF))
import           Data.Functor.Foldable      (Recursive (cata))
import           Data.List.NonEmpty         (nonEmpty)
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Ord                   (Down (..))
import           Data.Scientific            (toRealFloat)
import qualified Data.Text                  as T (Text, dropWhile, dropWhileEnd,
                                                  find)
import qualified Data.Tree                  as Tree (Tree (..))
import           Lens.Micro                 (Lens', lens, to, (.~), (^.))
import           Lens.Micro.TH              (makeLenses)
import           PPrint                     (PPrint (pprint))
import           Parser                     (Parser, comma, parserFromMaybe,
                                             separatedParser)
import           Safe.Foldable              (maximumMay, minimumMay)
import qualified Text.Megaparsec.Char.Lexer as L (scientific)
import           Todo                       (Todo, TodoTree, content, doneAt,
                                             doneAtToBool, makeTodoTreeValid)

newtype Name = Name T.Text deriving (Eq, Ord, Show)

runName :: Name -> T.Text
runName = coerce

getName :: T.Text -> Maybe Name
getName s
  | isNothing $ T.find (== '\n') s = Just . Name . T.dropWhileEnd (== ' ') $ T.dropWhile (== ' ') s
  | otherwise     = Nothing

instance PPrint Name where
  pprint name = runName name

newtype EaseOfImplement = EOI Float deriving (Eq, Ord, Show)

runEaseOfImplement :: EaseOfImplement -> Float
runEaseOfImplement = coerce

getEaseOfImplement :: Float -> Maybe EaseOfImplement
getEaseOfImplement n
  | n > 0 && n <= 10 = Just $ EOI n
  | otherwise = Nothing

newtype Impact = Impact Float deriving (Eq, Ord, Show)

runImpact :: Impact -> Float
runImpact = coerce

getImpact :: Float -> Maybe Impact
getImpact n
  | n > 0 && n <= 10 = Just $ Impact n
  | otherwise = Nothing

data QuickWinAnalysis = QWA
  { _name            :: Name
  , _easeOfImplement :: EaseOfImplement
  , _impact          :: Impact
  } deriving (Eq, Show)
makeLenses ''QuickWinAnalysis

instance Ord QuickWinAnalysis where
  a `compare` b = qwaToCriterion a `compare` qwaToCriterion b

instance PPrint QuickWinAnalysis where
  pprint qwa =
    runName (_name qwa) <>
    "," <>
    pprint (runEaseOfImplement (_easeOfImplement qwa)) <>
    "," <>
    pprint (runImpact (_impact qwa))

qwaToCriterion :: QuickWinAnalysis -> Down (Float, EaseOfImplement, Impact)
qwaToCriterion qwa =
  Down (runEaseOfImplement (_easeOfImplement qwa) * runImpact (_impact qwa), _easeOfImplement qwa, _impact qwa)

newtype QWATodoTree = QWATT (TodoTree QuickWinAnalysis)

runQWATodoTree :: QWATodoTree -> TodoTree QuickWinAnalysis
runQWATodoTree = coerce

instance PPrint QWATodoTree where
  pprint = pprint . runQWATodoTree

instance Eq QWATodoTree where
  (==) = (==) `on` runQWATodoTree

instance Ord QWATodoTree where
  compare = compare `on` runQWATodoTree

rootLabel :: Lens' (Tree.Tree a) a
rootLabel = lens Tree.rootLabel (\tree b -> Tree.Node b $ Tree.subForest tree)

subForest :: Lens' (Tree.Tree a) [Tree.Tree a]
subForest = lens Tree.subForest (Tree.Node . Tree.rootLabel)

makeQWATodoTreeValid :: Tree.Tree (Todo QuickWinAnalysis) -> QWATodoTree
makeQWATodoTreeValid tree =
  QWATT $ makeTodoTreeValid ((\f -> cata (\case
    NodeF root sub -> Tree.Node (f (getQuickest $ Tree.Node root sub) root) sub
   )) (\quickest -> (content . impact .~ (quickest ^. impact)) . (content . easeOfImplement .~ (quickest ^. easeOfImplement)))) tree
  where
    getQuickest :: Tree.Tree (Todo QuickWinAnalysis) -> QuickWinAnalysis
    getQuickest tree =
      cata (\case
        NodeF qwatodo quickests -> fromMaybe qwatodo $ minimumMay $ filter (^. doneAt . to doneAtToBool . to not) quickests
      ) tree
        ^. content

-- parser

nameParser :: Parser Name
nameParser = parserFromMaybe "fail with QuickWinAnalysis name parser." $
  getName <$> separatedParser

easeOfImplementParser :: Parser EaseOfImplement
easeOfImplementParser = parserFromMaybe "fail with QuickWinAnalysis ease of implement parser." $
  getEaseOfImplement . toRealFloat <$> L.scientific

impactParser :: Parser Impact
impactParser = parserFromMaybe "fail with QuickWinAnalysis impact parser." $
  getImpact . toRealFloat <$> L.scientific

qwaParser :: Parser QuickWinAnalysis
qwaParser = do
  name <- nameParser
  comma
  eoi <- easeOfImplementParser
  comma
  QWA name eoi <$> impactParser
