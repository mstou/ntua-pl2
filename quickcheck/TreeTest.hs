module TreeTest(prop_tree_trivial) where
  import Tree
  import Test.QuickCheck

  prop_tree_trivial t =
    collect (height t) $ t == t
      where types = (t :: Tree Int)
