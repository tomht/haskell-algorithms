binarysearch :: (Ord a) => [a] -> a -> Maybe Int
binarysearch [] x = Nothing
binarysearch x y = let l = length x
                       index = if l `mod` 2 == 0 then (l - 2) `div` 2 else (l - 1) `div` 2
                       value = x !! index
                   in if value == y
                      then Just index
                      else if value > y
                           then binarysearch (take index x) y
                           else case binarysearch (drop (index + 1) x) y of
                                     Nothing -> Nothing
                                     Just z -> Just (z + index + 1)
