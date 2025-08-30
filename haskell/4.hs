numElt xs = Prelude.length xs

numElt' xs = foldr (\ x y->y+1) 0 xs