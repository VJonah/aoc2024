-- | Day 2
import Data.List.Split
import Data.String.Utils

type Level = Int
type Report = [Level]

test1 :: [[Int]]
test1 = [[7,6,4,2,1],
         [1,2,7,8,9],
         [9,7,6,2,1],
         [1,3,2,4,5],
         [8,6,4,4,1],
         [1,3,6,7,9]]

isSafe :: Report -> Bool
isSafe report = ((all (<0) diffs || all (>0) diffs)) && (all inRange (map abs diffs))
  where diffs = map (uncurry (-)) (zip report (drop 1 report))
        inRange = (\x -> x >= 1 && x <= 3)

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

canBeSafe :: Report -> Bool
canBeSafe report = any isSafe [removeAt i report | i <- [0..len]]
  where len = length report

solve1 :: [Report] -> Int
solve1 reports = length (filter isSafe reports)

solve2 :: [Report] -> Int
solve2 reports = length (filter canBeSafe reports)

parseInput :: String -> IO [Report]
parseInput path = do content <- readFile path
                     return (map getLevels (getReports (strip content)))
                     where getLevels = (\report -> map (\level -> (read level :: Level)) (splitOn " " report))
                           getReports = splitOn "\n"

main :: IO ()
main = do putStrLn "Solution to part 1:"
          reports <- parseInput "input.txt"
          putStrLn (show (solve1 reports))
          putStrLn "Solution to part 2:"
          putStrLn (show (solve2 reports))
