{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ActionPriorityMatrix where
import           Data.Coerce                (coerce)
import           Data.Function              (on)
import           Data.Functor               (void)
import           Data.Functor.Classes       (Eq1, eq1)
import           Data.Maybe                 (isNothing)
import           Data.Proxy                 (Proxy (..))
import           Data.String                (fromString)
import qualified Data.Text                  as T (Text, dropWhile, dropWhileEnd,
                                                  find)
import           Data.Tree                  (Tree (..))
import           Data.Vector                (Vector, fromList)
import           GHC.Generics               (Generic)
import           Lens.Micro                 ((%~), (&))
import           Lens.Micro.TH              (makeLenses)
import           Parser                     (Parser, comma, parserFromMaybe,
                                             scn, separatedParser)
import           QuickWinAnalysis           (QuickWinAnalysis)
import           Text.Megaparsec            (anySingleBut, satisfy, some)
import           Text.Megaparsec.Char       (char, newline)
import qualified Text.Megaparsec.Char.Lexer as L (IndentOpt (..), float,
                                                  indentBlock)

newtype Name = Name T.Text deriving (Eq, Ord, Show)

runName :: Name -> T.Text
runName = coerce

getName :: T.Text -> Maybe Name
getName s
  | isNothing $ T.find (== '\n') s = Just . Name . T.dropWhileEnd (== ' ') $ T.dropWhile (== ' ') s
  | otherwise     = Nothing

newtype Impact = Impact Float deriving (Eq, Ord, Show)

runImpact :: Impact -> Float
runImpact = coerce

getImpact :: Float -> Maybe Impact
getImpact n
  | n > 0 && n <= 10 = Just $ Impact n
  | otherwise = Nothing

newtype Effort = Effort Float deriving (Eq, Ord, Show)

runEffort :: Effort -> Float
runEffort = coerce

getEffort :: Float -> Maybe Effort
getEffort n
  | n > 0 && n <= 10 = Just $ Effort n
  | otherwise = Nothing

data ActionPriorityMatrix qwa = APM
  { _name   :: Name
  , _impact :: Impact
  , _effort :: Effort
  , _qwas   :: Vector qwa
  } deriving (Eq, Show)
makeLenses ''ActionPriorityMatrix

-- APM's criterion

greatestImpact :: Impact
greatestImpact = Impact 10.0

greatestEffort :: Effort
greatestEffort = Effort 0.0

type ImpactRadius = Float
type EffortRadius = Float

type Radius = (ImpactRadius, EffortRadius)

apmToCriterion_ :: Impact -> Effort -> Radius -> ActionPriorityMatrix qwa -> Float
apmToCriterion_ impact effort (ir, er) apm =
  ((coerce (_impact apm) - coerce impact) * er) ** 2 + ((coerce (_effort apm) - coerce effort) * ir) ** 2

apmToCriterion :: ActionPriorityMatrix qwa -> Float
apmToCriterion apm
  | runImpact (_impact apm) < 5.0 = apmToCriterion_ greatestImpact greatestEffort (1, 2) (apm & impact %~ (Impact . (/ 2) . runImpact))
  | otherwise = apmToCriterion_ greatestImpact greatestEffort (1, 2) apm

instance Eq qwa => Ord (ActionPriorityMatrix qwa) where
  compare = compare `on` apmToCriterion

-- parser

nameParser :: Parser Name
nameParser = parserFromMaybe "fail with ActionPriorityMatrix name parser." $
  getName <$> separatedParser

impactParser :: Parser Impact
impactParser = parserFromMaybe "fail with ActionPriorityMatrix impact parser." $
  getImpact <$> L.float

effortParser :: Parser Effort
effortParser = parserFromMaybe "fail with ActionPriorityMatrix effort parser." $
  getEffort <$> L.float

apmParser :: Parser qwa -> Parser (ActionPriorityMatrix qwa)
apmParser pqwa = L.indentBlock scn $ do
  name <- nameParser
  comma
  impact <- impactParser
  comma
  effort <- effortParser
  pure $ L.IndentMany Nothing (pure . APM name impact effort . fromList) pqwa
