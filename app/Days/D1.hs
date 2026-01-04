module Days.D1 (p1) where

data Dir = Left | Right deriving (Show, Eq)

parseDir :: Char -> Dir
parseDir char = case char of
    'L' -> Days.D1.Left
    'R' -> Days.D1.Right
    _ -> error "Unexpected direction"

data Step = Step
    { dir :: Dir
    , num :: Int
    }
    deriving (Eq, Show)

parseLine :: String -> Step
parseLine line =
    Step thisDir thisNum
  where
    thisDir = parseDir (head line)
    thisNum = read (drop 1 line)

data State = State
    { count :: Int
    , position :: Int
    }

reduceState :: State -> Step -> State
reduceState state step = State newCount newPosition
  where
    newCount = count state + (if newPosition == 0 then 1 else 0)
    newPosition = case dir step of
        Days.D1.Left -> mod (position state - num step) 100
        Days.D1.Right -> mod (position state + num step) 100

parseLines :: String -> [Step]
parseLines inputs = map parseLine (lines inputs)

p1 :: String -> String
p1 inputs = (show . count) (foldl reduceState initState steps)
  where
    initState = State 0 50
    steps = parseLines inputs
