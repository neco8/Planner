module Todo (Todo (..)) where

data Todo s = Todo Bool s [Todo s] deriving (Eq, Ord, Show)
