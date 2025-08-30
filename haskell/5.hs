reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]


reverseList' xs = foldr (\ a acc -> acc++[a]) [] xs