module TreeTest(prop_tree_trivial,
                prop_tree_traversal_should_leave_height_unchanged,
                prop_tree_traversal_should_leave_size_unchanged,
                prop_tree_traversal_should_assign_all_numbers_from_1_up_to_size,
                extractVisitTimes
               ) where
  import Tree
  import Test.QuickCheck
  import Data.List

  extractVisitTimes (Node (_,x) []) = [x]
  extractVisitTimes (Node (_,x) xs) = x : remainingVisitTimes
    where remainingVisitTimes = foldl (++) [] (map extractVisitTimes xs)

  prop_tree_trivial t =
    collect (height t, size t) $ t == t
      where types = (t :: Tree Int)

  prop_tree_traversal_should_leave_height_unchanged tour t =
    height t == height (tour t)
      where types = (t :: Tree Int)

  prop_tree_traversal_should_leave_size_unchanged tour t =
    size t == size (tour t)
    where types = (t :: Tree Int)

  prop_tree_traversal_should_assign_all_numbers_from_1_up_to_size tour t =
    [1..n] \\ (extractVisitTimes $ tour t) == []
      where n = size t
            types = (t :: Tree Int)
