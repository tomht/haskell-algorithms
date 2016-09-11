quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort(filter (<=x) xs) ++ [x] ++ quicksort(filter (>x) xs)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort x =
  let s = bubble x
  in if s == x then s else bubblesort s

bubble :: (Ord a) => [a] -> [a]
bubble (x:y:xs) = if x <= y then x:(bubble (y:xs)) else y:(bubble (x:xs))
bubble x = x

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort x = foldl merge [] (map (\y -> [y]) x)

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = if x <= y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)
