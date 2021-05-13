module DFS(dfs) where

import Tree

-- | Function that annotates each node of a tree with the order in which
-- | a DFS traversal would visit it.
dfs :: Tree a -> Tree (a, Int)
dfs t = fst (aux 1 t)
  where aux :: Int -> Tree a ->
               (Tree (a, Int), Int)
        aux k (Node x ts) = (Node (x, k) ts', k')
          where (ts', k') = auxs (k+1) ts
        auxs :: Int -> [Tree a] ->
                ([Tree (a, Int)], Int)
        auxs k [] = ([], k)
        auxs k (t : ts) = (t' : ts', k'')
          where (t', k') = aux k t
                (ts', k'') = auxs k' ts
