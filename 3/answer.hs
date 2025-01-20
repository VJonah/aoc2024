-- | Day 3
import Data.List.Split
import Data.List
import Data.String.Utils
import Data.Char

test1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
test2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"


hasComma :: String -> Bool
hasComma "" = False
hasComma (ch:chs) | ch == ',' = True


isMul :: String -> Bool
isMul substr = first4 == "mul(" &&
               (last substr) == ')' &&
               hasComma args &&
               all isDigit left &&
               all isDigit right &&
               llen >= 1 && llen <= 3 &&
               rlen >= 1 && rlen <= 3
               where first4 = take 4 substr
                     args = drop 4 (take ((length substr)-1) substr)
                     [left,right] = splitOn "," args
                     llen = length left
                     rlen = length right

isDo :: String -> Bool
isDo substr = substr == "do()"

isDont :: String -> Bool
isDont substr = substr == "don't()"

parseMuls :: String -> [String]
parseMuls "" = []
parseMuls str = case mulExp of
                  Just exp -> case mulIdx of
                    Just idx -> exp:(parseMuls (drop (8+idx) str))
                  Nothing -> parseMuls (drop 1 str)
                where mulExp = find isMul possibleSubstrs
                      mulIdx = findIndex isMul possibleSubstrs
                      possibleSubstrs = [take x str | x <- [8..12]]

parseMem :: String -> Bool -> [String]
parseMem "" doing = []
parseMem str doing | doing = if isDont first7 then
                               parseMem (drop 7 str) False
                             else
                               case mulExp of
                                 Just exp -> case mulIdx of
                                   Just idx -> exp:(parseMem (drop (8+idx) str) doing)
                                 Nothing -> parseMem (drop 1 str) doing
                   | otherwise = if isDo first4 then
                                   parseMem (drop 4 str) True
                                else
                                   parseMem (drop 1 str) doing

  where first4 = take 4 str
        first7 = take 7 str
        mulExp = find isMul possibleSubstrs
        mulIdx = findIndex isMul possibleSubstrs
        possibleSubstrs = [take x str | x <- [8..12]]

parse :: String -> [String]
parse str = parseMem str True

evalMul :: String -> Int
evalMul mul = left * right
  where args = drop 4 (take ((length mul)-1) mul)
        [left, right] = map (\x -> read x :: Int) (splitOn "," args)

solve1 :: [String] -> Int
solve1 lines = sum (map evalMul (concat (map parseMuls lines)))

solve2 :: String -> Int
solve2 memory = sum (map evalMul (parse memory))

parseInput :: String -> IO [String]
parseInput path = do content <- readFile path
                     return (getLines (strip content))
                     where getLines = splitOn "\n"

main :: IO ()
main = do putStrLn "Solution to part 1:"
          lines <- parseInput "input.txt"
          putStrLn (show (solve1 lines))
          memory <- readFile "input.txt" -- We don't go line by line here
          putStrLn "Solution to part 2:"
          putStrLn (show (solve2 (strip memory)))
