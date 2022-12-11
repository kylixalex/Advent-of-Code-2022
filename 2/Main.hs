import System.IO
import Control.Exception

main = do
  withFile "input" ReadMode $ \handle -> do
    contents <- hGetContents handle
    ls <- evaluate $ lines contents
    putStrLn $ show $ solve1 ls
    putStrLn $ show $ solve2 ls

score :: String -> Int
score l = case l of "A X" -> 4
                    "A Y" -> 8
                    "A Z" -> 3
                    "B X" -> 1
                    "B Y" -> 5
                    "B Z" -> 9
                    "C X" -> 7
                    "C Y" -> 2
                    "C Z" -> 6
                    _ -> error "invalid format"

solve1 :: [String] -> Int
solve1 = sum . map score

solve2 :: [String] -> Int
solve2 = sum . map (score . transform)
  where
    transform :: String -> String
    transform l = case l of "A X" -> "A Z"
                            "A Y" -> "A X"
                            "A Z" -> "A Y"
                            "B X" -> "B X"
                            "B Y" -> "B Y" 
                            "B Z" -> "B Z"
                            "C X" -> "C Y"
                            "C Y" -> "C Z"
                            "C Z" -> "C X"
                            _ -> error "invalid format"
