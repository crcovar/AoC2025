import Data.Char
import Data.Function
import Data.List
import System.Environment

solve :: [String] -> Integer
solve ("+" : xs) = sum $ map read xs
solve ("*" : xs) = foldr ((*) . read) 1 xs

part1 :: [[String]] -> Integer
part1 = foldr ((+) . solve) 0

-- part2 :: [String] -> Integer
part2 l = foldr ((+) . solve) 0 equations
  where
    operands = words $ last l
    numbers = filter (/= [[]]) $ groupBy (\a b -> not (null a) && not (null b)) $ map (filter (`elem` ['0' .. '9'])) $ transpose $ init l
    equations = zipWith (:) operands numbers

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let input = map reverse $ transpose $ map words $ lines file
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 $ lines file
