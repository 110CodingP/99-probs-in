-- Symmetric binary trees.

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving Eq

instance Show a => Show (BinaryTree a) where
  show t = prettyPrint t 0
    where
      prettyPrint Nil lvl = "+" ++ (replicate (2*lvl) '-') ++ "." 
      prettyPrint (Node n Nil Nil) lvl = "+" ++ (replicate (2*lvl) '-') ++  (show n) 
      prettyPrint (Node n l r) lvl = "+" ++ (replicate (2*lvl) '-') ++ (show n) ++ "\n"
                                                                ++ (prettyPrint l (lvl+1)) ++ "\n" 
                                                                ++ (prettyPrint r (lvl+1)) 

mirror Nil Nil = True
mirror Nil _ = False
mirror _ Nil = False
mirror (Node a left_l right_l) (Node b left_r right_r) = (a == b) && mirror left_l right_r && mirror right_l left_r

isSymmetric (Node a left right) = mirror left right