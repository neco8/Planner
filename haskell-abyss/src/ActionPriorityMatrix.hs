{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ActionPriorityMatrix where
import           Data.Coerce          (coerce)
import           Data.Function        (on)
import           Data.Functor.Classes (Eq1, eq1)
import qualified Data.Text as T           (Text, lines)
import           Data.Tree            (Tree (..))
import           Data.Vector          (Vector)
import           GHC.Generics         (Generic)
import           Lens.Micro           ((%~), (&))
import           Lens.Micro.TH        (makeLenses)
import           QuickWinAnalysis     (QuickWinAnalysis)
import           Prelude              hiding (elem)

newtype Name = Name T.Text deriving (Eq, Ord, Show)

runName :: Name -> T.Text
runName = coerce

getName :: T.Text -> Maybe Name
getName s
  | length (T.lines s) /= 1 = Nothing
  | otherwise     = Just $ Name s

newtype Impact = Impact Float deriving (Eq, Ord, Show)

runImpact :: Impact -> Float
runImpact = coerce

newtype Effort = Effort Float deriving (Eq, Ord, Show)

runEffort :: Effort -> Float
runEffort = coerce

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

distanceFromGreatest_ :: Impact -> Effort -> Radius -> ActionPriorityMatrix qwa -> Float
distanceFromGreatest_ impact effort (ir, er) apm =
  ((coerce (_impact apm) - coerce impact) * er) ** 2 + ((coerce (_effort apm) - coerce effort) * ir) ** 2

distanceFromGreatest :: ActionPriorityMatrix qwa -> Float
distanceFromGreatest apm
  | runImpact (_impact apm) < 5.0 = distanceFromGreatest_ greatestImpact greatestEffort (1, 2) (apm & impact %~ (Impact . (/ 2) . runImpact))
  | otherwise = distanceFromGreatest_ greatestImpact greatestEffort (1, 2) apm

instance Eq qwa => Ord (ActionPriorityMatrix qwa) where
  compare = compare `on` distanceFromGreatest

apm = ActionPriorityMatrix (Name "name1") (Impact 10.0) (Effort 0.0) ["string"]
apm1 = ActionPriorityMatrix (Name "name1") (Impact 10.0) (Effort 10.0) ["string"]
apm2 = ActionPriorityMatrix (Name "name2") (Impact 5.0) (Effort 0.0) ["string"]
