module Main (main) where

import Data.IntMap (IntMap, assocs, elems, empty, insertWith)
import System.IO

main :: IO ()
main = do
    fishData <- readFile "./06/data.txt"
    let fishStr = map (\x -> read x :: Int) (items fishData)
    let fish = iterate update $ foldr (\k m -> insertWith (+) k 1 m) empty fishStr
    let simulateDays n = sum . elems . (!! n) $ fish
    putStrLn $ " 80 days: " ++ show (simulateDays 80)
    putStrLn $ "256 days: " ++ show (simulateDays 256)

items :: String -> [String]
items s = l : case s' of
    [] -> []
    (_:s'') -> items s''
  where (l, s') = break (== ',') s

update :: IntMap Int -> IntMap Int
update = foldr up empty . assocs
  where
    up = (\(k, x) m ->
        if k == 0
            then insertWith (+) 8 x (insertWith (+) 6 x m)
            else insertWith (+) (k - 1) x m)
