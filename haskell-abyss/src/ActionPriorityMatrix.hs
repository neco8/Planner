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
import           Data.Scientific            (toRealFloat)
import           Data.String                (fromString)
import qualified Data.Text                  as T (Text, dropWhile, dropWhileEnd,
                                                  find, intercalate, lines)
import           Data.Tree                  (Tree (..))
import           Data.Vector                (Vector, concatMap, fromList,
                                             toList)
import           GHC.Generics               (Generic)
import           Lens.Micro                 ((%~), (&))
import           Lens.Micro.TH              (makeLenses)
import           PPrint                     (PPrint, pprint)
import           Parser                     (Parser, comma, parserFromMaybe,
                                             scn, separatedParser)
import           Prelude                    hiding (concatMap)
import           QuickWinAnalysis           (QuickWinAnalysis)
import           Text.Megaparsec            (anySingleBut, satisfy, some)
import           Text.Megaparsec.Char       (char, newline)
import qualified Text.Megaparsec.Char.Lexer as L (IndentOpt (..), indentBlock,
                                                  scientific)

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

instance PPrint qwa => PPrint (ActionPriorityMatrix qwa) where
  pprint apm =
    join "\n"
    (runName (_name apm) <>
      "," <>
      pprint (runImpact (_impact apm)) <>
      "," <>
      pprint (runEffort (_effort apm)))
    (T.intercalate "\n" (toList (("  " <>) <$> concatMap (fromList . T.lines . pprint) (_qwas apm))))
    where
      join joiner a b
        | a /= mempty && b /= mempty = a <> joiner <> b
        | otherwise = a <> b

-- parser

nameParser :: Parser Name
nameParser = parserFromMaybe "fail with ActionPriorityMatrix name parser." $
  getName <$> separatedParser

impactParser :: Parser Impact
impactParser = parserFromMaybe "fail with ActionPriorityMatrix impact parser." $
  getImpact . toRealFloat <$> L.scientific

effortParser :: Parser Effort
effortParser = parserFromMaybe "fail with ActionPriorityMatrix effort parser." $
  getEffort . toRealFloat <$> L.scientific

apmParser :: Parser qwa -> Parser (ActionPriorityMatrix qwa)
apmParser pqwa = L.indentBlock scn $ do
  name <- nameParser
  comma
  impact <- impactParser
  comma
  effort <- effortParser
  pure $ L.IndentMany Nothing (pure . APM name impact effort . fromList) pqwa
