import Data.Char
import Data.List

-- comparing any 2 adjacent elements
checkAdjacent :: [Int] -> Bool
checkAdjancent [] = True
checkAdjacent [_] = True
checkAdjacent (x:y:xs)
  | abs (x - y) >= 1 && abs (x - y) <= 3 = checkAdjacent (y:xs)
  | otherwise = False

-- check if a list if ascending or descending
isAscDsc :: [Int] -> Bool 
isAscDsc xs = xs == sort xs || xs == reverse (sort xs)

-- combining two functions 
finalValidation :: [Int] -> Bool 
finalValidation xs = checkAdjacent xs && isAscDsc xs

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      listOfLists = map (map read . words) input :: [[Int]]
      results = map finalValidation listOfLists
      trueCount = length (filter id results)
  print trueCount
