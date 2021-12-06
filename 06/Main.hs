module Main (main) where

import Data.IntMap (IntMap, assocs, elems, empty, insertWith)
import System.IO

main :: IO ()
main = do
    fishData <- readFile "./06/data.txt"
    let fishStr = map (\x -> read x :: Int) (items fishData)
    let fish = foldl (\m k -> insertWith (+) k 1 m) empty fishStr
    let simulate n = sum . elems . (!! n) . iterate update
    putStrLn $ " 80 days: " ++ show (simulate 80 fish)
    putStrLn $ "256 days: " ++ show (simulate 256 fish)

items :: String -> [String]
items s = l : case s' of
    [] -> []
    (_:s'') -> items s''
  where (l, s') = break (== ',') s

update :: IntMap Int -> IntMap Int
update = foldl up empty . assocs
  where
    up = (\m (k, x) ->
        if k == 0
            then insertWith (+) 8 x . insertWith (+) 6 x $ m
            else insertWith (+) (k - 1) x m)
