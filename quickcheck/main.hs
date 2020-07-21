import Tree
import TreeTest
import BFS
import DFS

-- | Examples
t1 = Node 1 [ Node 2 [ Node 3 []
                     , Node 4 []
                     ]
            , Node 5 [ Node 6 [] ]
            ]

t2 = Node 'a' [ Node 'b' []
              , Node 'c' [ Node 'e' []
                         , Node 'f' []
                         ]
              , Node 'd' []
              ]

main = do
  print(t1)
  print(size(t1))
  print(dfs(t1))
  print(bfs(t1))
  putStrLn "---------"
  print(t2)
  print(size(t2))
  print(dfs(t2))
  print(bfs(t2))
