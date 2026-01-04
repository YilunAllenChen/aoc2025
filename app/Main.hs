module Main where

import Days.D1

main :: IO ()
main = do
    input <- readFile "data/d1"
    print (p1 input)
