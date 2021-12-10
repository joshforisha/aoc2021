module Main (main) where

import Data.Function ((&))
import Data.List
import Data.Maybe
import System.IO

main :: IO ()
main = do
    input <- readFile "./10/navigation.txt"
    let inLines = lines input
    let illegals = (catMaybes . map (snd . parse)) inLines
    putStrLn $ "Illegal points: " ++ show ((sum . map points) illegals)
    let incompletes = (map parse . (filter legal)) inLines
    putStrLn $ show ((middle . sort . map (completePoints . fst)) incompletes)

end :: Char -> Char
end '(' = ')'
end '[' = ']'
end '{' = '}'
end '<' = '>'
end _ = ' '

starts :: String
starts = "([{<"

parse :: String -> ([Char], Maybe Char)
parse line =
    foldl
        (\r@(es, ic) c ->
            case ic of
                Just _ -> r
                Nothing ->
                    if elem c starts
                        then (end c : es, Nothing)
                        else if c == head es
                            then (tail es, Nothing)
                            else (es, Just c)
        )
        ([], Nothing) line

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = 0

legal :: String -> Bool
legal = isJust . snd . parse

acPoints :: Char -> Int
acPoints ')' = 1
acPoints ']' = 2
acPoints '}' = 3
acPoints '>' = 4
acPoints _ = 0

completePoints :: String -> Int
completePoints = foldl (\t x -> t * 5 + acPoints x) 0

middle :: [a] -> a
middle [x] = x
middle xs = middle (init (tail xs))
