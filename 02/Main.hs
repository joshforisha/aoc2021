module Main (main) where

import System.IO

main :: IO ()
main = do
    course <- readFile "./02/course.txt"
    let directions = lines course
    let (hpos1, depth1) = position directions
    putStrLn $ "Position product: " ++ show (hpos1 * depth1)
    let (hpos2, depth2, _) = heading directions
    putStrLn $ "Heading product: " ++ show (hpos2 * depth2)

adjust :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
adjust (h, d, a) ("down", x) = (h, d, a + x)
adjust (h, d, a) ("forward", x) = (h + x, d + (x * a), a)
adjust (h, d, a) ("up", x) = (h, d, a - x)
adjust y _ = y

heading :: [String] -> (Int, Int, Int)
heading = foldl adjust (0, 0, 0) . map (pair . words)

pair :: [String] -> (String, Int)
pair [d, x] = (d, read x :: Int)

position :: [String] -> (Int, Int)
position = foldl step (0, 0) . map (pair . words)

step :: (Int, Int) -> (String, Int) -> (Int, Int)
step (h, d) ("down", x) = (h, d + x)
step (h, d) ("forward", x) = (h + x, d)
step (h, d) ("up", x) = (h, d - x)
step y _ = y
