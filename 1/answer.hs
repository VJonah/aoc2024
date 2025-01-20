-- | Day 1
import Data.List.Split
import Data.List
import Data.String.Utils

test1 = [[3,4,2,1,3,3],[4,3,5,3,9,3]]

solve1 :: [[Integer]] -> Integer
solve1 lists = sum (map absDiff (zip (sort left) (sort right)))
  where absDiff = (\(a,b) -> abs (a-b))
        [left,right] = lists

solve2 :: [[Integer]] -> Integer
solve2 lists = sum [x*(toInteger (length (filter (==x) right))) | x <- left]
  where [left,right] = lists

parseInput :: String -> IO [[Integer]]
parseInput path = do content <- readFile path
                     return (transpose (map splitLine (splitOn "\n" (strip content))))
                       where splitLine = (\line -> map (\x -> (read x :: Integer)) (splitOn "   " line))

main :: IO ()
main = do putStrLn "Solution to part 1:"
          lists <- parseInput "input.txt"
          putStrLn (show (solve1 lists))
          putStrLn "Solution to part 2:"
          putStrLn (show (solve2 lists))
