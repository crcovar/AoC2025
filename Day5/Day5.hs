import Data.Function
import Data.List
import System.Environment

wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsOn p s''
    where
      (w, s'') = break p s'

parseRangeTuple :: String -> (Integer, Integer)
parseRangeTuple r = (start, end)
  where
    (x : xs : _) = wordsOn (== '-') r
    start = read x :: Integer
    end = read xs :: Integer

parseRange :: String -> [Integer]
parseRange r = [start .. end]
  where
    (x : xs : _) = wordsOn (== '-') r
    start = read x :: Integer
    end = read xs :: Integer

inRanges :: [(Integer, Integer)] -> Integer -> Bool
inRanges [] _ = False
inRanges ((s, e) : xs) i
  | i >= s && i <= e = True
  | otherwise = inRanges xs i

rangeFinder :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
rangeFinder [] r = [r]
rangeFinder acc@((x, y) : xs) c@(a, b)
  | a >= x && a <= y = (x, max b y) : xs
  | otherwise = c : acc

part1 :: ([String], [String]) -> Int
part1 input = length $ filter (inRanges ranges) ids
  where
    ranges = map parseRangeTuple $ fst input
    ids = map read $ tail $ snd input

part2 :: ([String], [String]) -> Integer
part2 input = foldr (\(x, y) a -> a + y - x + 1) 0 $ foldl rangeFinder [] ranges
  where
    ranges = sortBy (compare `on` fst) $ map parseRangeTuple $ fst input

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let input = span (/= "") $ lines file
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
