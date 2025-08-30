elementAt xs 0 = error "Are you kidding me?"
elementAt xs n = 
    if length xs < n then error "There are not enough elements in the list"
    else xs !! (n-1)

elementAtSafe xs 0 = Nothing
elementAtSafe xs n = 
    if length xs < n then Nothing
    else Just (xs !! (n-1))