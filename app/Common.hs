module Common (Day (..), loadData, runDay) where

data Day = Day
    { day :: Int
    , p1' :: String -> String
    , p2' :: String -> String
    }

loadData :: Day -> IO String
loadData p = readFile ("data/d" ++ show (day p))

runDay :: Day -> IO ()
runDay currDay = do
    inputs <- loadData currDay
    print ("P1: " ++ p1' currDay inputs)
    print ("P2: " ++ p2' currDay inputs)
