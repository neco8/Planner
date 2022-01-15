{-# LANGUAGE OverloadedStrings #-}
module QuickWinAnalysis where
import           Data.Coerce (coerce)
import           Data.Maybe  (isNothing)
import qualified Data.Text   as T (Text, find)

newtype Name = Name T.Text deriving (Eq, Ord, Show)

runName :: Name -> T.Text
runName = coerce

getName :: T.Text -> Maybe Name
getName s
  | isNothing $ T.find (== '\n') s = Just $ Name s
  | otherwise     = Nothing

newtype EaseOfImplement = EaseOfImplement Float deriving (Eq, Ord, Show)

runEaseOfImplement :: EaseOfImplement -> Float
runEaseOfImplement = coerce

getEaseOfImplement :: Float -> Maybe EaseOfImplement
getEaseOfImplement n
  | n > 0 && n <= 10 = Just $ EaseOfImplement n
  | otherwise = Nothing

newtype Impact = Impact Float deriving (Eq, Ord, Show)

runImpact :: Impact -> Float
runImpact = coerce

getImpact :: Float -> Maybe Impact
getImpact n
  | n > 0 && n <= 10 = Just $ Impact n
  | otherwise = Nothing

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
