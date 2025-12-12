import Data.List
import System.Environment

type Point2 = [Int]

wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsOn p s''
    where
      (w, s'') = break p s'

parseLine :: [Char] -> Point2
parseLine s = map read $ wordsOn (== ',') s

area :: Point2 -> Point2 -> Int
area p q = w * h
  where
    (w : h : xs) = map ((+ 1) . abs) $ zipWith (-) p q

rects' :: Point2 -> [Point2] -> [Int]
rects' _ [] = []
rects' p (q : qs) = area p q : rects' p qs

rects :: [Point2] -> [Int]
rects [] = []
rects points = rects' p ps ++ rects ps
  where
    (p : ps) = points

part1 :: [Point2] -> Int
part1 = maximum . rects

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let input = map parseLine $ lines file
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
