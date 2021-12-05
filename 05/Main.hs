module Main (main) where

import Control.Monad
import Data.Map (Map, adjust, empty, insert, member, toList)
import System.IO

main :: IO ()
main = do
    ventsData <- readFile "./05/vents.txt"
    let ventLines = (map (((\[a,_,b] -> (a, b)) . words)) . lines) ventsData
    let ventPoints = join (map (\(a,b) -> linePoints (point a) (point b)) ventLines)
    putStrLn $ show (length (filter (\(_,l) -> l > 1) (toList . countPoints $ ventPoints)))

items :: String -> [String]
items "" = []
items s = l : case s' of
    [] -> []
    (_:s'') -> items s''
  where (l, s') = break (== ',') s

point :: String -> (Int, Int)
point str = (read a :: Int, read b :: Int)
  where [a, b] = items str

linePoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
linePoints (x1,y1) (x2,y2) =
    if (abs (x1 - x2)) == (abs (y1 - y2))
        then zip [x1, x1 + (signum $ x2 - x1)..x2] [y1, y1 + (signum $ y2 - y1)..y2]
        else if x1 == x2
            then zip [x1,x2..] [(min y1 y2)..(max y1 y2)]
            else if y1 == y2
                then zip [(min x1 x2)..(max x1 x2)] [y1,y2..]
                else []

countPoints :: [(Int, Int)] -> Map (Int,Int) Int
countPoints =
    foldl (\ts p -> if member p ts then adjust (+ 1) p ts else insert p 1 ts) empty
