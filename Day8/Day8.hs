import Data.Function
import Data.List
import Data.Maybe
import Data.Set qualified as Set
import System.Environment

type Point3 = (Double, Double, Double)

wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsOn p s''
    where
      (w, s'') = break p s'

makeTriple :: [a] -> (a, a, a)
makeTriple x = (a, b, c)
  where
    (a : b : c : _) = x

d :: Point3 -> Point3 -> Double
d (px, py, pz) (qx, qy, qz) = sqrt $ (px - qx) ^ 2 + (py - qy) ^ 2 + (pz - qz) ^ 2

makePairs :: [Point3] -> [(Point3, Point3)]
makePairs [] = []
makePairs (x : xs) = p' ++ makePairs xs
  where
    p' = map (x,) xs

makeCircuit :: [Set.Set Point3] -> (Double, (Point3, Point3)) -> [Set.Set Point3]
makeCircuit acc (_, (p, q)) = Set.insert p (Set.insert q c) : circuits
  where
    s' = find (\s -> Set.member p s || Set.member q s) acc
    c = fromMaybe Set.empty s'
    circuits = case s' of
      Just s -> delete s acc
      Nothing -> acc

combineCircuits :: [Set.Set Point3] -> [Set.Set Point3]
combineCircuits = foldl combine' []

combine' :: [Set.Set Point3] -> Set.Set Point3 -> [Set.Set Point3]
combine' [] a = [a]
combine' (b : bs) a
  | Set.intersection a b /= Set.empty = combine' bs (Set.union a b)
  | otherwise = combine' bs a ++ [b]

-- sortBy (flip compare) $ map Set.size $
part1 :: [[String]] -> Int
part1 input = product $ take 3 $ sortBy (flip compare) $ map Set.size $ combineCircuits $ foldl makeCircuit [] $ take 1000 $ sortBy (compare `on` fst) withDistance
  where
    junctionBoxes = map (makeTriple . map read) input
    pairs = makePairs junctionBoxes
    withDistance = map (\(p, q) -> (d p q, (p, q))) pairs

solve :: (Double, (Point3, Point3)) -> Integer
solve (_, ((x, _, _), (x', _, _))) = floor x * floor x'

stepCircuits :: [Set.Set Point3] -> [(Double, (Point3, Point3))] -> (Double, (Point3, Point3))
stepCircuits circuits (p : ps)
  | length c == 1 = p
  | otherwise = stepCircuits c ps
  where
    c = combineCircuits $ makeCircuit circuits p

part2 :: [[String]] -> Integer
part2 input = solve $ stepCircuits seed $ drop 1000 withDistance
  where
    junctionBoxes = map (makeTriple . map read) input
    seed = combineCircuits $ foldl makeCircuit circuits $ take 1000 $ sortBy (compare `on` fst) withDistance
    circuits = map Set.singleton junctionBoxes
    pairs = makePairs junctionBoxes
    withDistance = sort $ map (\(p, q) -> (d p q, (p, q))) pairs

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn "Part 1"
  print $ part1 $ map (wordsOn (== ',')) $ lines file
  putStrLn "Part 2"
  print $ part2 $ map (wordsOn (== ',')) $ lines file
