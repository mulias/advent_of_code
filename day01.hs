import           Control.Exception
import           Data.Function

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

main = do
  puzzleInput <- readFile "puzzle_inputs/day01.txt"
  let nums = puzzleInput & split '\n' & fmap read
      part1 = head [i * j | i <- nums, j <- nums, i + j == 2020]
      part2 = head [a * b * c | a <- nums, b <- nums, c <- nums, a + b + c == 2020]
      _ = assert $ part1 == 1006875
      _ = assert $ part2 == 165026160
   in
      putStrLn $
        "Part 1: " ++ show part1
        ++ "\n" ++
        "Part 2: " ++ show part2
