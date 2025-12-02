import Data.List
import Data.Set qualified as Set

wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsOn p s''
    where
      (w, s'') = break p s'

parseRange :: String -> [Integer]
parseRange r =
  let (x : xs : _) = wordsOn (== '-') r
      start = read x :: Integer
      end = read xs :: Integer
   in [start .. end]

-- isRepeating :: Integer -> Bool
-- isRepeating i =
--   let s = show i
--       xs = tail s ++ s
--    in case s `elemIndex` xs of
--       Nothing -> False
--       Just i' -> i' < len s

main = do
  file <- readFile "example.txt"
  let ranges = map parseRange $ wordsOn (== ',') file
  print $ ranges
  putStrLn "Part 1"
  print "Not yet solved"
  putStrLn "Part 2"
  print "Not yet solved"
