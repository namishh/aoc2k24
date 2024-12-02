import Data.List
import Data.Char

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      pairs = map words input
      (one, two) = unzip $ map (\[x, y] -> (read x :: Int, read y :: Int)) pairs -- FUCKERY
      sone = sort one
      stwo = sort two

  let mapped = zipWith (\x y -> abs (x - y)) sone stwo 
  print (sum mapped)
