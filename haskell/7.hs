data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten xs = case xs of
    Elem a -> [a]
    List xs -> foldl (++) [] (map flatten xs)


