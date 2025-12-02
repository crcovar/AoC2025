import Data.List
import Data.Set qualified as Set

wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsOn p s''
    where
      (w, s'') = break p s'

group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' n l
  | n > 0 = (take n l):(group' n (drop n l))
  | otherwise = [l]

parseRange :: String -> [Integer]
parseRange r = [start .. end]
  where (x : xs : _) = wordsOn (== '-') r
        start = read x :: Integer
        end = read xs :: Integer

isRepeatingTwice :: Integer -> Bool
isRepeatingTwice i
  | x == y = True
  | otherwise = False
  where s = show i
        (x,y) = splitAt (length s `div` 2) s

isRepeating :: Int -> Integer -> Bool
isRepeating j i
  | j > h = False
  | n == 0 = True
  | otherwise = isRepeating (j+1) i
  where s = show i
        h = length s `div` 2
        (x:xs) = group' j s
        n = length $ filter (/=x) xs

part1 :: [[Integer]] -> Integer
part1 = sum . concat . map (filter isRepeatingTwice)

part2 :: [[Integer]] -> Integer
part2 = sum . concat . map (filter (isRepeating 1))

main = do
  file <- readFile "input.txt"
  let input = map parseRange $ wordsOn (== ',') file
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
