import Data.List

joltage :: Int -> [Char] -> String
joltage 0 _ = []
joltage x rack = b : joltage (x - 1) (drop (i + 1) rack)
  where
    r = drop (x - 1) $ reverse rack
    b = minimumBy (flip compare) r
    Just i = elemIndex b rack

part1 :: [String] -> Integer
part1 = sum . map (read . joltage 2)

part2 :: [String] -> Integer
part2 = sum . map (read . joltage 12)

main = do
  file <- readFile "input.txt"
  let l = lines file
  putStrLn "Part 1"
  print $ part1 l
  putStrLn "Part 2"
  print $ part2 l
