import Data.Function
import Data.List
import Data.Map qualified as Map
import System.Environment
import Tree qualified

type Coord = (Int, Int)

type GridPoint = (Coord, Char)

makeGrid' :: Int -> [Char] -> [GridPoint]
makeGrid' _ [] = []
makeGrid' y l = ((y, x), c) : makeGrid' y cs
  where
    (c : cs) = l
    x = length l - 1

makeGrid :: [String] -> [[GridPoint]]
makeGrid [] = []
makeGrid rows = makeGrid' y r : makeGrid rs
  where
    (r : rs) = rows
    y = length rows - 1

buildTree :: Coord -> Tree.Tree Coord -> Tree.Tree Coord
buildTree c@(y, x) t
  | isMember = Tree.insert (y, x + 1) $ Tree.insert (y, x - 1) t
  | otherwise = t
  where
    isMember = Tree.memberBy (compare `on` snd) c t
    Just b = Tree.findBy (compare `on` snd) c t

split' :: Coord -> (Int, [Int]) -> (Int, [Int])
split' (y, x) acc@(c, b)
  | x `elem` b = (c + 1, x + 1 : x - 1 : filter (/= x) b)
  | otherwise = acc

quantumSplit :: Coord -> Tree.Tree Int -> Tree.Tree Int
quantumSplit (y, x) timelines = case n' of
  Just (c, a) -> Tree.insertWith c (a - 1) $ Tree.insertWith c (a + 1) $ Tree.remove a timelines
  Nothing -> timelines
  where
    n' = Tree.find x timelines

part1 :: [GridPoint] -> Int
part1 input = fst $ foldr split' (0, [snd start]) points
  where
    Just (start, _) = find (\(_, v) -> v == 'S') input
    points = reverse $ dropWhile (\(y, _) -> y >= fst start) $ map fst input

part2 :: [GridPoint] -> Int
part2 input = Tree.size $ foldr quantumSplit (Tree.singleton (snd start)) points
  where
    Just (start, _) = find (\(_, v) -> v == 'S') input
    points = reverse $ dropWhile (\(y, _) -> y >= fst start) $ map fst input

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn "Part 1"
  print $ part1 $ concatMap (filter (\(_, v) -> v /= '.')) $ makeGrid $ lines file
  putStrLn "Part 2"
  print $ part2 $ concatMap (filter (\(_, v) -> v /= '.')) $ makeGrid $ lines file
