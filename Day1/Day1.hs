import Data.List

parseLines :: [String] -> [(Char, Integer)]
parseLines = map (\(x : xs) -> (x, read xs))

turn :: Integer -> (Char, Integer) -> (Integer, Integer)
turn acc (d, x)
  | x > 100 = turn acc (d, x - 100)
turn acc ('L', x)
  | (acc - x) < 0 = (100 + acc - x, acc)
  | otherwise = (acc - x, acc)
turn acc ('R', x)
  | (acc + x) > 99 = (acc + x - 100, acc)
  | otherwise = (acc + x, acc)

spin :: [(Char, Integer)] -> (Integer, [Integer])
spin = mapAccumL turn 50

turnFoldL :: (Integer, Integer) -> (Char, Integer) -> (Integer, Integer)
turnFoldL acc (_, 0) = acc
turnFoldL (0, z) ('L', x) = turnFoldL (99, z + 1) ('L', x - 1)
turnFoldL (0, z) ('R', x) = turnFoldL (1, z + 1) ('R', x - 1)
turnFoldL (99, z) ('R', x) = turnFoldL (0, z) ('R', x - 1)
turnFoldL (a, z) ('L', x) = turnFoldL (a - 1, z) ('L', x - 1)
turnFoldL (a, z) ('R', x) = turnFoldL (a + 1, z) ('R', x - 1)

main = do
  file <- readFile "input.txt"
  let part1 = uncurry (:) . spin . parseLines $ lines file
  print "Part 1"
  print $ length $ filter (== 0) part1
  print "Part 2"
  let l = foldl turnFoldL (50, 0) . parseLines $ lines file
  print $ snd l
