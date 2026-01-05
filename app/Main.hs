module Main where

import Days.D1

main :: IO ()
main = do
    input <- readFile "data/d1"
    print ("P1: " ++ p1 input)
    print ("P2: " ++ p2 input)
