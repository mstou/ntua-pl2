module BFS(bfs) where

import Tree

-- | Function that annotates each node of a tree with the order in which
-- | a BFS traversal would visit it.
bfs :: Tree a -> Tree (a, Integer)
bfs t = t'
  where (t', ks') = aux ks t
        ks = 1 : ks'
        aux (k : ks) (Node x ts) = (Node (x, k) ts', (k+1) : ks')
          where (ts', ks') = auxs ks ts
        auxs ks [] = ([], ks)
        auxs ks (t : ts) = (t' : ts', ks'')
          where (t', ks') = aux ks t
                (ts', ks'') = auxs ks' ts
