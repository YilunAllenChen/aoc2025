module Days.D2 (d2) where

import Common (Day (..))

p1 :: String -> String
p1 inputs = (show . count) (foldl reduceState initState steps)
  where
    initState = State 0 50
    steps = parseLines inputs

p2 :: String -> String
p2 inputs = (show . count) (foldl reduceState2 initState steps)
  where
    initState = State 0 50
    steps = parseLines inputs

d2 :: Day
d2 = Day 2 p1 p2
