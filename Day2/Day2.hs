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
parseRange r =
  let (x : xs : _) = wordsOn (== '-') r
      start = read x :: Integer
      end = read xs :: Integer
   in [start .. end]

isRepeatingTwice :: Integer -> Bool
isRepeatingTwice i
  | odd (length s) = False
  | x == y = True
  | otherwise = False
  where s = show i
        (x,y) = splitAt (length s `div` 2) s

isRepeating :: Int -> String -> Bool
isRepeating _ [] = True
isRepeating j s
  | j > h = False
  | n == 0 = True
  | otherwise = isRepeating (j+1) s
  where
        h = length s `div` 2
        (x:xs) = group' j s
        n = length $ filter (/=x) xs

part1 :: String -> Integer
part1 = sum . concat . map (filter isRepeatingTwice . parseRange) . wordsOn (== ',')

part2 :: String -> Integer
part2 = sum . concat . map (filter (isRepeating 1 . show) . parseRange) . wordsOn (== ',')

main = do
  file <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 file
  putStrLn "Part 2"
  print $ part2 file
