module Tree (Tree(Node), size) where

  -- | A polymorphic n-ary tree data structure
  data Tree a = Node a [Tree a]
    deriving (Show, Eq)

  size (Node _ []) = 1
  size (Node x xs) = 1 + childrenNodes
    where childrenSizes = map size xs
          childrenNodes = foldl (+) 0 childrenSizes
