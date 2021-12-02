module Main (main) where

import Data.Function
import System.IO

main :: IO ()
main = do
    course <- readFile "./02/course.txt"
    let directions = lines course
    putStrLn $ "Position product: " ++ show (positionProduct directions)
    putStrLn $ "Heading product: " ++ show (headingProduct directions)

pair :: [String] -> (String, Int)
pair [d, x] = (d, read x :: Int)

step :: (String, Int) -> (Int, Int)
step ("down", x) = (0, x)
step ("forward", x) = (x, 0)
step ("up", x) = (0, -x)
step (_, _) = (0, 0)

positionProduct :: [String] -> Int
positionProduct directions =
  let
    (hpos, depth) = directions
        & map (step . pair . words)
        & foldr1 (\(h, d) (hx, dx) -> (h + hx, d + dx))
  in
    hpos * depth

adjust :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
adjust (h, d, a) ("down", x) = (h, d, a + x)
adjust (h, d, a) ("up", x) = (h, d, a - x)
adjust (h, d, a) ("forward", x) = (h + x, d + (x * a), a)
adjust y _ = y

headingProduct :: [String] -> Int
headingProduct directions =
  let
    (hpos, depth, _) = directions & map (pair . words) & foldl adjust (0, 0, 0)
  in
    hpos * depth
