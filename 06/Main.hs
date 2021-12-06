module Main (main) where

import Data.Function ((&))
import Data.Map (Map)
import System.IO
import qualified Data.Map as M

type Fish = Map Int Int

main :: IO ()
main = do
    fishData <- readFile "./06/data.txt"
    let fishStr = map (\x -> read x :: Int) (items fishData)
    let fish = foldl (\m k -> M.insertWith (+) k 1 m) M.empty fishStr
    let simulate n = (!!n) . iterate update
    putStrLn $ " 80 days: " ++ (show . population . simulate 80) fish
    putStrLn $ "256 days: " ++ (show . population . simulate 256) fish

items :: String -> [String]
items s = l : case s' of
    [] -> []
    (_:s'') -> items s''
  where (l, s') = break (== ',') s

population :: Fish -> Int
population = sum . M.elems

update :: Fish -> Fish
update = foldl up M.empty . M.assocs
  where
    up = (\m (k, x) ->
        if k == 0
            then (M.insert 8 x . M.insert 6 x) m
            else M.insertWith (+) (k - 1) x m)
