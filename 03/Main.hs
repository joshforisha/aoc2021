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

if' :: Bool -> a -> a -> a
if' true yes no = if true then yes else no

more :: Int -> Bool -> Int -> Char
more len high x = if' (x >= (len - x)) (if' high '1' '0') (if' high '0' '1')

toDecimal :: String -> Int
toDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

powerRates :: [String] -> (Int, Int)
powerRates readings =
  let
    totals = foldl (zipWith bump) [0,0..] readings
    len = length readings
    gammaString = map (more len True) totals
    epsilonString = map (more len False) totals
  in
    (toDecimal gammaString, toDecimal epsilonString)

rating :: Bool -> [String] -> Int -> Int
rating _ [] _ = 0
rating _ [reading] _ = toDecimal reading
rating high readings i =
  let crit = more (length readings) high (foldl (bumpf i) 0 readings)
  in rating high (filter (\r -> (r !! i) == crit) readings) (i + 1)
