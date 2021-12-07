module Main (main) where

import System.IO

main :: IO ()
main = do
    posStr <- readFile "./07/crabs.txt"
    let positions = (map (\x -> read x :: Int) . items) posStr
    let highestPos = maximum positions
    putStrLn $ show (minimum (map (\d -> fuel d positions) [0..highestPos]))
    putStrLn $ show (minimum (map (\d -> crabFuel d positions) [0..highestPos]))

items :: String -> [String]
items s = l : (if null s' then [] else items (tail s'))
  where (l, s') = break (== ',') s

fuel :: Int -> [Int] -> Int
fuel d = (sum . map (\x -> abs (x - d)))

crabFuel :: Int -> [Int] -> Int
crabFuel d = (sum . map (\x -> sum [1..abs (x - d)]))
