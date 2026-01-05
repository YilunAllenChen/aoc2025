module Days.D1 (p1, p2) where

data Dir = DLeft | DRight deriving (Show, Eq)

data Step = Step
    { dir :: Dir
    , num :: Int
    }
    deriving (Eq, Show)

data State = State
    { count :: Int
    , position :: Int
    }
    deriving (Eq, Show)

parseDir :: Char -> Dir
parseDir char = case char of
    'L' -> DLeft
    'R' -> DRight
    _ -> error "Unexpected direction"

parseLine :: String -> Step
parseLine line =
    let thisDir = case line of
            [] -> error "Empty line!"
            hd : _ -> parseDir hd
        thisNum = read (drop 1 line)
     in Step thisDir thisNum

runStep :: Int -> Step -> Int
runStep curr step = case dir step of
    DLeft -> curr - num step
    DRight -> curr + num step

reduceState :: State -> Step -> State
reduceState state step = State newCount newPosition
  where
    newCount = count state + (if newPosition == 0 then 1 else 0)
    newPositionUnrounded = runStep (position state) step
    newPosition = case dir step of
        DLeft -> mod newPositionUnrounded 100
        DRight -> mod newPositionUnrounded 100

divEuclid :: Int -> Int -> Int
divEuclid a b = fst (divMod a b)

reduceState2 :: State -> Step -> State
reduceState2 state step =
    let oldPos = position state
        newPos = runStep oldPos step
        zeros = case dir step of
            DLeft -> divEuclid (oldPos - 1) 100 - divEuclid (newPos - 1) 100
            DRight -> divEuclid newPos 100
     in State (count state + zeros) (mod newPos 100)

parseLines :: String -> [Step]
parseLines inputs = map parseLine (lines inputs)

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
