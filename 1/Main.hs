import System.IO
import Data.List
import Data.List.Split
import Control.Exception

main =
  withFile "input" ReadMode $ \handle -> do
    contents <- hGetContents handle
    elves <- evaluate $ map (sum . map read . words) $ splitOn "\n\n" contents
    putStrLn $ show $ solve1 elves
    putStrLn $ show $ solve2 elves

solve1 :: [Int] -> Int
solve1 = maximum

solve2 :: [Int] -> Int
solve2 = sum . take 3 . sortBy (flip compare)
