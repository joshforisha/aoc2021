module Day17 where

import Control.Monad (join)
import Data.Maybe
import System.IO

type Position = (Int, Int)
type TargetArea = (Position, Position)
type Velocity = (Int, Int)

main :: IO ()
main = do
    target <- parseInput <$> readFile "./17/input.txt"
    let velocities = join (map (\x -> map (\y -> (x, y)) [-200..200]) [0..125])
    let matches = shootMatches target velocities
    putStrLn $ "Vertical peak: " ++ show (maximum (map snd matches))
    putStrLn $ "# matches: " ++ show (length matches)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy y x = l : (if null s' then [] else splitBy y (tail s'))
  where (l, s') = break (== y) x

parseInput :: String -> TargetArea
parseInput str = ((read minX :: Int, read maxX :: Int), (read minY :: Int, read maxY :: Int))
  where
    [[minX, _, maxX], [minY, _, maxY]] = (map (splitBy '.' . drop 3) . splitBy ',' . drop 12 . init) str

step :: (Velocity, Position) -> (Velocity, Position)
step ((vx, vy), (x, y)) = ((maximum [0, vx - 1], vy - 1), (x + vx, y + vy))

shootAt :: TargetArea -> Position -> Int -> Velocity -> Maybe Int
shootAt t@((minX, maxX), (minY, maxY)) position highest velocity =
  let
    (newVelocity, p@(x, y)) = step (velocity, position)
    newHighest = maximum [highest, y]
  in
    if x >= minX && x <= maxX && y >= minY && y <= maxY
        then Just newHighest
        else if x > maxX || y < minY
            then Nothing
            else shootAt t p newHighest newVelocity

shootMatches :: TargetArea -> [Velocity] -> [(Velocity, Int)]
shootMatches t =
    map (\(v, h) -> (v, fromJust h)) . filter (isJust . snd) . map (\v -> (v, shootAt t (0, 0) 0 v))
