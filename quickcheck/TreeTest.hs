module TreeTest(prop_tree_trivial,
                prop_tree_traversal_should_leave_height_unchanged,
                prop_tree_traversal_should_leave_size_unchanged,
                prop_tree_traversal_should_assign_all_numbers_from_1_up_to_size,
                extractVisitTimes,
                extractChildrenVisitTimes,
                runAllTraversalTests,
                runBFStests,
                runDFStests
               ) where
  import Tree
  import Test.QuickCheck
  import Data.List

  extractVisitTimes (Node (_,x) []) = [x]
  extractVisitTimes (Node (_,x) xs) = x : remainingVisitTimes
    where remainingVisitTimes = foldl (++) [] (map extractVisitTimes xs)

  extractChildrenVisitTimes [] = []
  extractChildrenVisitTimes l = aux l []
    where aux [] ts = reverse ts
          aux ((Node (x,n) l) : cs) ts = aux cs (n:ts)

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

  prop_tree_traversal_should_assign_1_to_root tour t =
    root_number == 1
    where (Node (_, root_number) _) = tour t
          types = (t :: Tree Int)

  prop_dfs_should_visit_the_whole_subtree_first dfs (Node _ []) = True
  prop_dfs_should_visit_the_whole_subtree_first dfs t =
    childrenVisitTimes == dfs_order
    where (Node (x,n) children) = dfs t
          childrenVisitTimes = extractChildrenVisitTimes children
          childrenSizes = map size children

          depth_first_order [_] times = reverse times
          depth_first_order (h:hs) (t:ts) = depth_first_order hs ((h+t):t:ts)

          dfs_order = depth_first_order childrenSizes [2]
          types = (t :: Tree Int)

  prop_bfs_should_visit_children_first bfs (Node _ []) = True
  prop_bfs_should_visit_children_first bfs t =
    childrenVisitTimes == [2..(numChildren+1)]
    where (Node (x,n) children) = bfs t
          childrenVisitTimes = extractChildrenVisitTimes children
          numChildren = length children


  runAllTraversalTests numTests tour = do
      putStrLn("Cheking property prop_tree_traversal_should_leave_height_unchanged: ")
      quickCheckWith stdArgs { maxSuccess = numTests } (prop_tree_traversal_should_leave_height_unchanged tour)
      putStrLn("")

      putStrLn("Cheking property prop_tree_traversal_should_leave_size_unchanged: ")
      quickCheckWith stdArgs { maxSuccess = numTests } (prop_tree_traversal_should_leave_size_unchanged tour)
      putStrLn("")


      putStrLn("Cheking property prop_tree_traversal_should_assign_all_numbers_from_1_up_to_size: ")
      quickCheckWith stdArgs { maxSuccess = numTests } (prop_tree_traversal_should_leave_height_unchanged tour)
      putStrLn("")


      putStrLn("Cheking property prop_tree_traversal_should_assign_1_to_root: ")
      quickCheckWith stdArgs { maxSuccess = numTests } (prop_tree_traversal_should_assign_1_to_root tour)
      putStrLn("")


  runBFStests numTests bfs = do
    runAllTraversalTests numTests bfs

    putStrLn("Cheking property prop_bfs_should_visit_children_first: ")
    quickCheckWith stdArgs { maxSuccess = numTests } (prop_bfs_should_visit_children_first bfs)
    putStrLn("")

  runDFStests numTests dfs = do
    runAllTraversalTests numTests dfs

    putStrLn("Cheking property prop_dfs_should_visit_the_whole_subtree_first: ")
    quickCheckWith stdArgs { maxSuccess = numTests } (prop_dfs_should_visit_the_whole_subtree_first dfs)
    putStrLn("")
