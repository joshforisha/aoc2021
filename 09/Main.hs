module Main (main) where

import Control.Monad (join)
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (sort)
import Data.Maybe (catMaybes)
import System.IO

type Height  = Int
type Heights = [[Int]]
type Pos     = (Int, Int)

main :: IO ()
main = do
    input <- readFile "./09/map.txt"
    let hs = input & lines & map (map digitToInt)
    let lowPositions = lowPoints hs
    putStrLn $ "# low points: " ++ show (lowPositions & map (risk hs) & sum)
    let basins = map (basin hs []) lowPositions
    let largestBasinsProduct = basins & map length & sort & reverse & take 3 & product
    putStrLn $ "Product of 3 largest basins: " ++ show largestBasinsProduct

map' :: (Int -> a -> b) -> [a] -> [b]
map' f = zipWith f [0..]

height :: Heights -> Pos -> Height
height hs (x, y) = hs !! y !! x

neighbors :: Heights -> Pos -> [Pos]
neighbors hs (x, y) =
  let
    mx = (length . head) hs - 1
    my = length hs - 1
  in
    [ if y > 0  then Just (x, y - 1) else Nothing
    , if x < mx then Just (x + 1, y) else Nothing
    , if y < my then Just (x, y + 1) else Nothing
    , if x > 0  then Just (x - 1, y) else Nothing
    ] & catMaybes

isLowPoint :: Heights -> Pos -> Bool
isLowPoint hs p = neighbors hs p & map (height hs) & all (> height hs p)

lowPoints :: Heights -> [Pos]
lowPoints hs = map' (\y -> map' (\x _ -> (x, y))) hs & map (filter (isLowPoint hs)) & join

risk :: Heights -> Pos -> Int
risk = ((1 +) .) . height

basin :: Heights -> [Pos] -> Pos -> [Pos]
basin hs bs p@(x, y) =
    if height hs p < 9 && not (elem p bs)
        then foldl (basin hs) (p:bs) (neighbors hs p)
        else bs
