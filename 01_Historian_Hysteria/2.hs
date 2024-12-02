import Data.Char
import Data.List (elemIndices)

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x xs = length $ elemIndices x xs 

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      pairs = map words input
      (one, two) = unzip $ map (\[x, y] -> (read x :: Int, read y :: Int)) pairs -- FUCKERY
      result = sum [x * countOccurrences x two | x <- one]
  print result