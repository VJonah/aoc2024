-- | Day 4

import Data.List.Split
import Data.String.Utils

type Index = (Int,Int)
type Puzzle = [[Char]]

test1 :: Puzzle
test1 = ["MMMSXXMASM",
         "MSAMXMSMSA",
         "AMXSXMAAMM",
         "MSAMASMSMX",
         "XMASAMXAMM",
         "XXAMMXXAMA",
         "SMSMSASXSS",
         "SAXAMASAAA",
         "MAMMMXMMMM",
         "MXMXAXMASX"]

getAt :: Puzzle -> Index -> Char
getAt puzzle (x,y) = if x >= 0 && x <= width &&
                        y >= 0 && y <= height then
                       puzzle !! y !! x
                     else
                       '\0'
  where height = (length puzzle) - 1
        width = (length (head puzzle)) - 1

dirs :: Index -> [[Index]]
dirs (x,y) = [[(x,y-j) | j <- rng], -- up
              [(x,y+j) | j <- rng], -- down
              [(x-j,y) | j <- rng], -- left
              [(x+j,y) | j <- rng], -- right
              [(x-j,y-j) | j <- rng], -- upper left diag
              [(x+j,y-j) | j <- rng], -- upper right diag
              [(x+j,y+j) | j <- rng], -- bottom right diag
              [(x-j,y+j) | j <- rng]] -- bottom left diag
             where rng = [0..3]

countXmassAt :: Puzzle -> Index -> Int
countXmassAt puzzle idx = length (filter isXmas (map (map (getAt puzzle)) (dirs idx)))
  where isXmas = (\str -> str == "XMAS" || str == "SMAX")

solve1 :: Puzzle -> Int
solve1 puzzle = sum (map (countXmassAt puzzle) xs)
  where xs = filter isX [(x,y) | x <- widths, y <- heights]
        heights = [0..((length puzzle)-1)]
        widths = [0..(length (head puzzle))]
        isX = (\idx -> (getAt puzzle idx) == 'X')

cross :: Index -> [[Index]]
cross (x,y) = [[(x-1,y-1), (x,y), (x+1,y+1)], -- left diagonal
               [(x+1,y-1), (x,y), (x-1,y+1)]] -- right diagonal

countXMASAt :: Puzzle -> Index -> Int
countXMASAt puzzle idx | isXMAS crossAtIdx = 1
                       | otherwise = 0
  where isXMAS = (\[left,right] -> isMAS left && isMAS right)
        isMAS = (\str -> str == "MAS" || str == "SAM")
        crossAtIdx = (map (map (getAt puzzle)) (cross idx))

solve2 :: Puzzle -> Int
solve2 puzzle = sum (map (countXMASAt puzzle) as)
  where as = filter isA [(x,y) | x <- widths, y <- heights]
        heights = [0..((length puzzle)-1)]
        widths = [0..(length (head puzzle))]
        isA = (\idx -> (getAt puzzle idx) == 'A')

parseInput :: String -> IO Puzzle
parseInput path = do content <- readFile path
                     return (getLines (strip content))
                     where getLines = splitOn "\n"

main :: IO ()
main = do putStrLn "Solution to part 1:"
          puzzle <- parseInput "input.txt"
          putStrLn (show (solve1 puzzle))
          putStrLn "Solution to part 2:"
          putStrLn (show (solve2 puzzle))
