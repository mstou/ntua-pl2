import Tree
import TreeTest
import BFS
import Test.QuickCheck


main = do
  putStrLn("Testing the implementation of BFS\n")
  runBFStests 1000 bfs
