module Main (main) where

import Data.Char (digitToInt)
import Data.List (foldl')
import System.IO

main :: IO ()
main = do
    report <- readFile "./03/report.txt"
    let readings = lines report
    let (gamma, epsilon) = powerRates readings
    putStrLn $ "Power Consumption: " ++ show (gamma * epsilon)
    let oxygen = rating True readings 0
    let co2 = rating False readings 0
    putStrLn $ "Life Support: " ++ show (oxygen * co2)

bump :: Int -> Char -> Int
bump t r = if r == '1' then t + 1 else t

bumpf :: Int -> Int -> String -> Int
bumpf i t rs = if rs !! i == '1' then t + 1 else t

most :: Int -> Bool -> Int -> Char
most len high x =
    if x >= (len - x)
        then if high then '1' else '0'
        else if high then '0' else '1'

toDecimal :: String -> Int
toDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

powerRates :: [String] -> (Int, Int)
powerRates readings =
  let
    totals = foldl (zipWith bump) [0,0..] readings
    len = length readings
  in
    ( toDecimal $ map (most len True) totals
    , toDecimal $ map (most len False) totals
    )

rating :: Bool -> [String] -> Int -> Int
rating _ [] _ = 0
rating _ [reading] _ = toDecimal reading
rating high readings i =
  let
    crit = most (length readings) high (foldl (bumpf i) 0 readings)
  in
    rating high (filter (\r -> (r !! i) == crit) readings) (i + 1)
