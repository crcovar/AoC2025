import System.Environment
import qualified Data.Map as Map

type Coord = (Int, Int)
type GridPoint = (Coord, Char)

makeGrid' :: Int -> [Char] -> [GridPoint]
makeGrid' _ [] = []
makeGrid' y l = ((y, x), c) : makeGrid' y cs
  where
    (c:cs) = l
    x = length l - 1

makeGrid :: [String] -> [[GridPoint]]
makeGrid [] = []
makeGrid rows = makeGrid' y r : makeGrid rs
  where
    (r:rs) = rows
    y = length rows - 1

neighbors :: Coord -> Map.Map Coord Char -> [Maybe Char]
neighbors (y,x) m =
  [Map.lookup (y, x+1) m
  , Map.lookup (y, x-1) m
  , Map.lookup (y+1, x) m
  , Map.lookup (y+1, x+1) m
  , Map.lookup (y+1, x-1) m
  , Map.lookup (y-1, x) m
  , Map.lookup (y-1, x+1) m
  , Map.lookup (y-1, x-1) m
  ]

canMove :: Map.Map Coord Char -> Coord -> Char -> Bool
canMove m k _ = i > 4
  where i =  length . filter (== Nothing) $ neighbors k m

move :: Map.Map Coord Char -> Int
move m
  | x == Map.empty = 0
  | otherwise = Map.size x + move xs
  where
    (x,xs) =  Map.partitionWithKey (canMove m) $ m

part1 :: String -> Int
part1 s = Map.size . Map.filterWithKey (canMove m) $ m
  where m = Map.filter (/= '.') . Map.fromList . concat . makeGrid $ lines s

part2 :: String -> Int
part2 s = move . Map.filter (/= '.') . Map.fromList . concat . makeGrid $ lines s

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn "Part 1"
  print $ part1 file
  putStrLn "Part 2"
  print $ part2 file
