import Tree
import TreeTest
import DFS
import Test.QuickCheck


main = do
  putStrLn("Testing the implementation of DFS\n")
  runDFStests 1000 dfs
