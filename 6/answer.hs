-- | Day 6

import Data.List
import Data.List.Split
import Data.String.Utils

type Coord = (Int,Int)
type Map = [String]


findGuard :: String -> Int
findGuard row = case findIndex (=='^') row of
                    Just idx -> idx
                    Nothing -> -1

parseInput :: [String] -> (Map, Coord)
parseInput lines = (lines, guardCoord)
  where guardCoord = case findIndex (>=0) guardIndexes of
                       Just idx -> ((guardIndexes !! idx), idx)
        guardIndexes = map findGuard lines



readLines :: String -> IO [String]
readLines path = do content <- readFile path
                    return (splitOn "\n" (strip content))

main :: IO ()
main = do lines <- readLines "test1.txt"
          let (map,guard) = parseInput lines
          putStrLn (show guard)
