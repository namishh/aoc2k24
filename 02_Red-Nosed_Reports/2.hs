import Data.Char
import Data.List

-- comparing any 2 adjacent elements
checkAdjacent :: [Int] -> Bool
checkAdjancent [] = True
checkAdjacent [_] = True
checkAdjacent (x:y:xs)
  | abs (x - y) >= 1 && abs (x - y) <= 3 = checkAdjacent (y:xs)
  | otherwise = False

-- generate all lists with 1 element removed
removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (y:ys) = ys : map (y :) (removeOne ys)

-- check if a list if ascending or descending
isAscDsc :: [Int] -> Bool 
isAscDsc xs = xs == sort xs || xs == reverse (sort xs)

-- combining two functions 
finalValidation :: [Int] -> Bool 
finalValidation xs = checkAdjacent xs && isAscDsc xs

finalfinalValidation :: [Int] -> Bool
finalfinalValidation xs
  | finalValidation xs = True
  | otherwise = any finalValidation (removeOne xs)


main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      listOfLists = map (map read . words) input :: [[Int]]
      results = map finalfinalValidation listOfLists
      trueCount = length (filter id results)
  print trueCount
