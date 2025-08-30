-- from 9.hs
inAcc x xs = null xs || (x == head xs)
groupList xs = groupList' xs [] [] 
    where
        groupList' [x] acc list =  if inAcc x acc then list++[acc++[x]] else (list++[acc])++[[x]]
        groupList' (x:xs) acc list =
            if  inAcc x acc
            then 
                groupList' xs (acc++[x]) list 
            else 
                groupList' xs [x] (list++[acc])

encode xs = map (\a -> (length a, head a)) (groupList xs)