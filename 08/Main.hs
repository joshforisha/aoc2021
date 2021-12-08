module Main (main) where

import Control.Monad
import Data.List
import System.IO

main :: IO ()
main = do
    input <- readFile "./08/input.txt"
    let dlines = (map (\l -> (words (head l), words (last l))) . map (split '|') . lines) input
    let signals = (filter (\w -> length w /= 5 && length w /= 6) . join . map snd) dlines
    putStrLn $ "Number of 1/3/4/7: " ++ show (length signals)
    putStrLn $ show ((sum . map process) dlines)

split :: Char -> String -> [String]
split c s = l : (if null s' then [] else split c (tail s'))
  where (l, s') = break (== c) s

common :: String -> String -> Int
common xs ys =
    foldr (\x z -> if elem x ys then z + 1 else z) 0 xs

findByCommon :: Int -> String -> [String] -> String
findByCommon c key keys =
    maybe "" id (find (\k -> common key k == c) keys)

findByLength :: Int -> [String] -> String
findByLength len keys =
    maybe "" id (find ((== len) . length) keys)

process :: ([String], [String]) -> Int
process (ks, cs) =
  let
    keys = map sort ks
    codes = map sort cs
    one = findByLength 2 keys
    four = findByLength 4 keys
    seven = findByLength 3 keys
    eight = findByLength 7 keys
    fiveKeys = filter ((== 5) . length) keys
    three = findByCommon 2 one fiveKeys
    fiveKeys' = filter (/= three) fiveKeys
    sixKeys = filter ((== 6) . length) keys
    nine = findByCommon 4 four sixKeys
    sixKeys' = filter (/= nine) sixKeys
    zero = findByCommon 2 one sixKeys'
    [six] = filter (/= zero) sixKeys'
    five = findByCommon 5 six fiveKeys'
    [two] = filter (/= five) fiveKeys'
  in
    read (map (\code ->
        if code == zero then '0'
        else if code == one then '1'
        else if code == two then '2'
        else if code == three then '3'
        else if code == four then '4'
        else if code == five then '5'
        else if code == six then '6'
        else if code == seven then '7'
        else if code == eight then '8'
        else if code == nine then '9'
        else ' '
        ) codes) :: Int
