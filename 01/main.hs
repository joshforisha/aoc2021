module Main (main) where

import System.IO

main :: IO ()
main = do
    depthData <- readFile "./01/depths.txt"
    let depths = map (\x -> read x :: Int) (lines depthData)
    putStrLn $ "Depth increases: " ++ show (increases depths)
    putStrLn $ "3-Windowed increases: " ++ show (windowedIncreases 3 depths)

bump :: Bool -> Int
bump true = if true then 1 else 0

increases :: [Int] -> Int
increases (x:xs) =
    fst $ foldl (\(inc, a) b -> (inc + (bump $ b > a), b)) (0, x) xs

windowed :: Int -> [a] -> [[a]]
windowed _ [] = []
windowed size ls =
    if length ls >= size
        then (take size ls) : windowed size (tail ls)
        else windowed size (tail ls)

windowedIncreases :: Int -> [Int] -> Int
windowedIncreases size xs =
  let
    (w:ws) = map (foldr (+) 0) (windowed size xs)
  in
    fst $ foldl (\(inc, a) b -> (inc + (bump $ b > a), b)) (0, w) ws
