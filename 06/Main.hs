module Main (main) where

import Data.Map (Map, adjust, assocs, empty, insert, insertWith, member)
import System.IO
import qualified Data.Map as M

main :: IO ()
main = do
    fishData <- readFile "./06/data.txt"
    let fish = map (\x -> read x :: Int) (items fishData)
    let fishMap = foldl (\m x -> if member x m then adjust (+ 1) x m else insert x 1 m) empty fish
    putStrLn $ "80 days: " ++ show (population (simulate fishMap 80))
    putStrLn $ "256 days: " ++ show (population (simulate fishMap 256))

items :: String -> [String]
items s = l : case s' of
    [] -> []
    (_:s'') -> items s''
  where (l, s') = break (== ',') s

population :: Map Int Int -> Int
population = sum . M.elems

update :: Map Int Int -> Map Int Int
update =
    foldl (\m (k, x) ->
        if k == 0
            then insert 6 x (insert 8 x m)
            else let k' = k - 1 in
                if member k' m
                    then adjust (+ x) k' m
                    else insert k' x m
        ) empty . assocs

simulate :: Map Int Int -> Int -> Map Int Int
simulate fish 0 = fish
simulate fish days = simulate (update fish) (days - 1)
