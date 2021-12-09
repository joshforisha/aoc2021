module Main (main) where

import Control.Monad (join)
import Data.Char (digitToInt)
import Data.Function ((&))
import System.IO

main :: IO ()
main = do
    input <- readFile "./09/test.txt"
    let heights = (map (map digitToInt) . lines) input
    let maxX = length (heights !! 0) - 1
    let maxY = length heights - 1
    let h = heightAt heights
    let heightPairs =
            walk2 (\x y -> (h x y, neighbors x y heights)) maxX maxY
            & join
    let lowPoints =
            filter
                (\(j, ns) -> foldl (\yes n -> yes && j < n) True ns)
                heightPairs
            & map ((+ 1) . fst)
            & sum
    putStrLn $ show lowPoints

heightAt :: [[Int]] -> Int -> Int -> Int
heightAt hs x y = (hs !! y) !! x

walk :: (Int -> a) -> Int -> [a]
walk f to = map (\x -> f x) [0..to]

walk2 :: (Int -> Int -> a) -> Int -> Int -> [[a]]
walk2 f toX toY =
    map (\y -> map (\x -> f x y) [0..toX]) [0..toY]

neighbors :: Int -> Int -> [[Int]] -> [Int]
neighbors x y heights =
  let
    h = heightAt heights
    maxX = length (heights !! y) - 1
    maxY = length heights - 1
  in
    [ if x > 0 then Just (h (x - 1) y) else Nothing
    , if y < maxY then Just (h x (y + 1)) else Nothing
    , if x < maxX then Just (h (x + 1) y) else Nothing
    , if y > 0 then Just (h x (y - 1)) else Nothing
    ] & filter (\x -> case x of
        Nothing -> False
        Just _ -> True
    ) & map (\x -> case x of
        Nothing -> 0
        Just y -> y
    )
