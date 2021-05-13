import Tree
import Test.QuickCheck

wrong :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy

merge :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
merge f (Node x tsx) (Node y tsy) = Node (f x y) $ mergedChildren
  where commonMerged = zipWith (merge f) tsx tsy

        keepAfter 0 l = l
        keepAfter n (x:xs) = keepAfter (n-1) xs

        numx = length tsx
        numy = length tsy

        restChildren =
          if numx > numy
            then keepAfter numy tsx
            else keepAfter numx tsy

        mergedChildren =
          if numx == numy
            then commonMerged
            else commonMerged ++ restChildren


sumTree :: (Tree Int) -> Int
sumTree (Node x []) = x
sumTree (Node x children) = x + (foldr (+) 0 $ map sumTree children)

prop_merge_with_sum_should_maintain_the_total_sum mergeSum t1 t2 =
  (sumTree merged) == (sumTree t1) + (sumTree t2)
  where merged = mergeSum t1 t2
        types = (t1 :: Tree Int, t2 :: Tree Int)

exampleTree :: Tree Int
exampleTree = Node 1 [Node 1 []]

main = do
  putStrLn("Checking the correct implementation of merge:")
  quickCheckWith stdArgs { maxSuccess = 10000 } (prop_merge_with_sum_should_maintain_the_total_sum $ merge (+))
  putStrLn("\n")
  putStrLn("Checking the wrong implementation of merge:")
  quickCheckWith stdArgs { maxSuccess = 10000 } (prop_merge_with_sum_should_maintain_the_total_sum $ wrong (+))
