-- | Day 5

import Data.Set
import Data.List.Split
import Data.String.Utils
import Data.List

type Constraint = (Int, Int)
type Constraints = Set Constraint
type Update = [Int]
type Updates = [Update]

toInt :: String -> Int
toInt x = read x :: Int

parseConstraint :: String -> Constraint
parseConstraint line = (y,x)
  where [x,y] = Data.List.map toInt (splitOn "|" line)

parseUpdate :: String -> Update
parseUpdate line = Data.List.map toInt (splitOn "," line)

parseInput :: [String] -> (Constraints, Updates)
parseInput lines = (fromList (Data.List.map parseConstraint constraints), Data.List.map parseUpdate updates)
  where constraints = Data.List.take blankIdx lines
        updates = Data.List.drop (blankIdx+1) lines
        blankIdx = case Data.List.findIndex (=="") lines of
                        Just idx -> idx

readLines :: String -> IO [String]
readLines path = do content <- readFile path
                    return (splitOn "\n" (strip content))

orderings :: Update -> [(Int,Int)]
orderings [_] = []
orderings (x:xs) = [(x,y) | y <- xs] ++ orderings xs

isValid :: Constraints -> Update -> Bool
isValid constraints update = not (any isIn (orderings update))
  where isIn = (\pair -> member pair constraints)



middle :: [a] -> a
middle xs = Data.List.head (Data.List.drop half xs)
  where half = div (length xs) 2

solve1 :: Constraints -> Updates -> Int
solve1 constraints updates = sum (Data.List.map middle (Data.List.filter (isValid constraints) updates))

-- correct :: Constraints -> Update -> Update
-- correct constraints update = Data.List.head (Data.List.filter (isValid constraints) (permutations update))

swap :: Int -> Int -> Update -> Update
swap x y update = (takeWhile (/=x) update) ++ [y,x] ++ (tail (dropWhile (/=y) update))

correct :: Constraints -> Update -> Update
correct constraints update = case find notValid (orderings update) of
                                Just (x,y) -> correct constraints (swap x y update)
                                Nothing -> update
  where notValid = (\ord -> member ord constraints)

-- merge :: Ord a => [a] -> [a] -> [a]
-- merge [] ys = ys
-- merge xs [] = xs
-- merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
--                     | otherwise = y:(merge (x:xs) ys)

-- mergeSort :: Ord a => [a] -> [a]
-- mergeSort [x] = [x]
-- mergeSort xs = merge (mergeSort firstHalf)  (mergeSort secondHalf)
--   where (firstHalf, secondHalf) = Data.List.splitAt (div (length xs) 2) xs

consMerge :: Constraints -> Update -> Update -> Update
consMerge _ [] ys = ys
consMerge _ xs [] = xs
consMerge cons (x:xs) (y:ys) | (member (x,y) cons) = y:(consMerge cons (x:xs) ys)
                             | otherwise = x:(consMerge cons xs (y:ys))

updateSort :: Constraints -> Update -> Update
updateSort _ [x] = [x]
updateSort cons xs = consMerge cons (updateSort cons firstHalf) (updateSort cons secondHalf)
  where (firstHalf, secondHalf) = Data.List.splitAt (div (length xs) 2) xs

solve2 :: Constraints -> Updates -> Int
solve2 constraints updates = sum (Data.List.map middle corrected)
  where corrected = Data.List.map (updateSort constraints) (Data.List.filter notValid updates)
        notValid = (\up -> not (isValid constraints up))

main :: IO ()
main = do putStrLn "Solution to part 1:"
          lines <- readLines "input.txt"
          let (constraints, updates) = parseInput lines
          putStrLn (show (solve1 constraints updates))
          putStrLn "Solution to part 2:"
          putStrLn (show (solve2 constraints updates))
