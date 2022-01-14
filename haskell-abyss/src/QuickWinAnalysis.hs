module QuickWinAnalysis where
import Data.Coerce (coerce)

newtype Name = Name String deriving (Eq, Ord, Show)

newtype EaseOfImplement = EaseOfImplement Float deriving (Eq, Ord, Show)

runEaseOfImplement :: EaseOfImplement -> Float
runEaseOfImplement = coerce

newtype Impact = Impact Float deriving (Eq, Ord, Show)

runImpact :: Impact -> Float
runImpact = coerce

data QuickWinAnalysis = QuickWinAnalysis
  { _name            :: Name
  , _easeOfImplement :: EaseOfImplement
  , _impact          :: Impact
  } deriving (Eq, Show)

instance Ord QuickWinAnalysis where
  a `compare` b = qwaToCriterion a `compare` qwaToCriterion b

qwaToCriterion :: QuickWinAnalysis -> (Float, EaseOfImplement, Impact)
qwaToCriterion qwa =
  (runEaseOfImplement (_easeOfImplement qwa) * runImpact (_impact qwa), _easeOfImplement qwa, _impact qwa)