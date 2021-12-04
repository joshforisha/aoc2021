module Main (main) where

import Control.Monad
import System.IO

type Board = [[Int]]

main :: IO ()
main = do
    puzzle <- readFile "./04/puzzle.txt"
    let (ds:bs) = lines puzzle
    let draws = map (\x -> read x :: Int) (items ds)
    let boards = splitBoards bs
    let boardData = map (analyze draws) boards
    putStrLn $ "First winning score: " ++ show ((snd . maximum) boardData)
    putStrLn $ "Last winning score: " ++ show ((snd . minimum) boardData)

items :: String -> [String]
items "" = []
items s =
  let
    (l, s') = break (== ',') s
  in
    l : case s' of
        [] -> []
        (_:s'') -> items s''

splitBoards :: [String] -> [Board]
splitBoards [] = []
splitBoards boardLines =
  let
    (b, qs) = splitAt 5 (tail boardLines)
    board = map (\l -> map (\x -> read x :: Int) (words l)) b
  in
    [board] ++ splitBoards qs

checkLine :: [Int] -> [Int] -> Bool
checkLine _ [] = True
checkLine draws (x:xs) = elem x draws && checkLine draws xs

checkBoard :: [Int] -> Board -> Bool
checkBoard draws rows =
  let
    column i = foldl (\xs x -> xs ++ [x !! i]) [] rows
  in
    length (filter (checkLine draws) (rows ++ map column [0..4])) > 0

winDrawsTo :: [Int] -> Board -> Int -> [Int]
winDrawsTo draws board x =
  let
    xs = take x draws
  in
    if checkBoard xs board
        then xs
        else winDrawsTo draws board (x + 1)

analyze :: [Int] -> Board -> (Int, Int)
analyze draws board =
  let
    winDraws = winDrawsTo draws board 5
    score = sum (filter (not . flip elem winDraws) (join board)) * last winDraws
  in
    (length winDraws, score)
