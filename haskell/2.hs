myButLast [] = error "No end"
myButLast [x] = error "No end"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs