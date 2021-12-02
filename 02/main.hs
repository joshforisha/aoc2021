module Main (main) where

import Data.Function
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
adjust (h, d, a) ("up", x) = (h, d, a - x)
adjust (h, d, a) ("forward", x) = (h + x, d + (x * a), a)
adjust y _ = y

heading :: [String] -> (Int, Int, Int)
heading = foldl adjust (0, 0, 0) . map (pair . words)

pair :: [String] -> (String, Int)
pair [d, x] = (d, read x :: Int)

position :: [String] -> (Int, Int)
position = foldr1 (\(h, d) (hx, dx) -> (h + hx, d + dx)) . map (step . pair . words)

step :: (String, Int) -> (Int, Int)
step ("down", x) = (0, x)
step ("forward", x) = (x, 0)
step ("up", x) = (0, -x)
step (_, _) = (0, 0)
