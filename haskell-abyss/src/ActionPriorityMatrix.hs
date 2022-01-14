{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ActionPriorityMatrix where
import QuickWinAnalysis (QuickWinAnalysis)
import Data.Functor.Classes (Eq1, eq1)
import GHC.Generics (Generic)
import Data.Tree (Tree (..))
import Data.Coerce (coerce)
import Data.Function (on)
import Lens.Micro ((%~), (&))
import Lens.Micro.TH (makeLenses)
import Data.Text (Text)

newtype Name = Name String deriving (Eq, Ord, Show)

runName :: Name -> String
runName = coerce

getName :: String -> Maybe Name
getName s
  | '\n' `elem` s = Nothing
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
  , _qwas   :: [qwa]
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
