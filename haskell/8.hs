consecutivePairs xs = zip (init xs) (tail xs)
groupedList xs = foldl (\x y -> if (fst y /= snd y) then x++[fst y] else x ) [] (consecutivePairs xs) ++ [last xs]