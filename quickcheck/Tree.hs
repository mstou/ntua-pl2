module Tree (Tree(Node), size, height, leaves) where
  import Test.QuickCheck

  -- | A polymorphic n-ary tree data structure
  data Tree a = Node a [Tree a]
    deriving (Show, Eq)

  -- | Helping functions that compute the size and height of a tree
  size (Node _ []) = 1
  size (Node x xs) = 1 + childrenNodes
    where childrenSizes = map size xs
          childrenNodes = foldl (+) 0 childrenSizes

  height (Node _ []) = 1
  height (Node x xs) = 1 + foldl max 1 (map height xs)

  leaves (Node _ []) = 1
  leaves (Node x xs) = foldl (+) 0 $ map leaves xs

  -- | Implementing Arbitrary for Tree
  instance Arbitrary a => Arbitrary (Tree a) where

    arbitrary = sized arbitraryTreeWithSize
      where arbitraryTreeWithSize 0 =
                do x <- arbitrary
                   return (Node x [])

            arbitraryTreeWithSize n =
                do x <- arbitrary
                   maxNumberOfChildren <- choose(1, n-1)
                   childrenSizes <- chooseChildrenSizes maxNumberOfChildren (n-1)
                   children <- sequence $ [arbitraryTreeWithSize] <*> childrenSizes
                   return (Node x children)

            -- Chooses up to n numbers s.t. they add up to something smaller than k
            chooseChildrenSizes n k  = chooseChildrenAux n k []
              where chooseChildrenAux 0 _ l =
                      return l

                    chooseChildrenAux _ 0 l =
                      return l

                    chooseChildrenAux n k l =
                      do x <- choose (0, k)
                         xs <- chooseChildrenAux (n-1) (k-x) (x:l)
                         return xs

    shrink (Node _ []) = []
    shrink (Node x children) = keepTheRoot ++ keepAChild ++ keepAllChilds
      where
          keepTheRoot = [(Node x [])]
          keepAChild  = children -- quickcheck will recursively call shrink on them
          keepAllChilds = [(Node x shrinkedChildren) | shrinkedChildren <- mapM shrink children]
