{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ActionPriorityMatrix where
import           Data.Coerce          (coerce)
import           Data.Function        (on)
import           Data.Functor.Classes (Eq1, eq1)
import           Data.String          (IsString, fromString)
import qualified Data.Text            as T (Text, find)
import           Data.Tree            (Tree (..))
import           Data.Vector          (Vector)
import           GHC.Generics         (Generic)
import           Lens.Micro           ((%~), (&))
import           Lens.Micro.TH        (makeLenses)
import           QuickWinAnalysis     (QuickWinAnalysis)
import Data.Maybe (isNothing)

newtype Name = Name T.Text deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . fromString

runName :: Name -> T.Text
runName = coerce

getName :: T.Text -> Maybe Name
getName s
  | isNothing $ T.find (== '\n') s = Just $ Name s
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

data ActionPriorityMatrix qwa = ActionPriorityMatrix
  { _name   :: Name
  , _impact :: Impact
  , _effort :: Effort
  , _qwas   :: Vector qwa
  } deriving (Eq, Show)
makeLenses ''ActionPriorityMatrix

-- calc distance from greatest APM

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

apm = ActionPriorityMatrix (Name "name1") (Impact 10.0) (Effort 0.0) ["string"]
apm1 = ActionPriorityMatrix (Name "name1") (Impact 10.0) (Effort 10.0) ["string"]
apm2 = ActionPriorityMatrix (Name "name2") (Impact 5.0) (Effort 0.0) ["string"]
