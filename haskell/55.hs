import Data.List (nub)
-- Construct completely balanced binary trees.

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving Eq

instance Show a => Show (BinaryTree a) where
  show t = prettyPrint t 0
    where
      prettyPrint Nil lvl = "+" ++ (replicate (2*lvl) '-') ++ "." 
      prettyPrint (Node n Nil Nil) lvl = "+" ++ (replicate (2*lvl) '-') ++  (show n) 
      prettyPrint (Node n l r) lvl = "+" ++ (replicate (2*lvl) '-') ++ (show n) ++ "\n"
                                                                ++ (prettyPrint l (lvl+1)) ++ "\n" 
                                                                ++ (prettyPrint r (lvl+1)) 

addNode b n (Node a Nil Nil) = Node a (Node b Nil Nil) Nil: [Node a Nil (Node b Nil Nil)]
addNode b n (Node a Nil right) = [Node a (Node b Nil Nil) right]
addNode b n (Node a left Nil) = [Node a left (Node b Nil Nil)]
addNode b n (Node a left right) = 
    if n >= 4 
    then 
        [Node a l right | l <- addNode b (div n 2) left]++[Node a left r | r <- addNode b (div n 2) right]
    else 
        []

cbalTree 1 = [ Node 1 Nil Nil ]
cbalTree n = nub ( concatMap (addNode 1 n) (cbalTree (n-1)))