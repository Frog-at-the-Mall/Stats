-- janky stats formulas for and by mattFrye 022021

import Data.List
import Data.Char
import Data.Map (insertWith', empty, filter, elems, keys)


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

mean xs = realToFrac (sum xs) / genericLength xs

median xs = let a = quicksort(xs)in a !! ceiling(genericLength xs / 2)



midrange xs = (minimum xs + maximum xs)/2

--measures of dispersion

range xs = let a = quicksort xs in maximum a - minimum a

---xxxxx

sumofsqrs xs = realToFrac(sum (map (^2) xs)) - (realToFrac(sum xs))^2  /genericLength xs


