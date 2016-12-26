module Tree where

import Control.Monad.Cont


-- Tree traversals

data Tree a
  = EmptyTree
  | Tree { root :: Node a}

data Node a
  = Node {
    value :: a,
    children :: [Node a] }


-- Recursive DFS

treeWalkR :: Tree a -> [a]
treeWalkR EmptyTree = []
treeWalkR (Tree root) = treeWalkR' root

treeWalkR' :: Node a -> [a]
treeWalkR' (Node v children) = v : concatMap treeWalkR' children

-- Heap-based stack DFS

treeWalkH :: Tree a -> [a]
treeWalkH EmptyTree = []
treeWalkH (Tree root) = treeWalkH' root

treeWalkH' :: Node a -> [a]
treeWalkH' = reverse . loop [] []
  where
    loop out []     (Node v [])     = v:out -- End of traversal
    loop out stack  (Node v (c:cs)) = loop (v:out) (toStack (cs:stack)) c
    loop out (h:t)  (Node v [])     = loop (v:out) (toStack (tail h:t)) (head h)
    toStack t = dropWhile null t


-- Continuation passing style

treeWalkC :: Tree a -> [a]
treeWalkC EmptyTree = []
treeWalkC (Tree root) = treeWalkC' root

treeWalkC' :: Node a -> [a]
treeWalkC' = loop id
  where
    loop :: ([a] -> [a]) -> Node a -> [a]
    loop cont (Node v cs) =
      let conts = foldr (\n comp r -> loop (comp . (r ++)) n)
                        cont
                        cs
      in conts [v]


-- With continuation Monad

treeWalkM :: Tree a -> [a]
treeWalkM EmptyTree = []
treeWalkM (Tree root) = treeWalkM' root

treeWalkM' :: Node a -> [a]
treeWalkM' n = runCont (loop n) id
  where
    loop :: Node a -> Cont [a] [a]
    loop (Node v cs) = do
      rs <- mapM loop cs
      return (v : concat rs)


--

generateNode :: Int -> Node Int
generateNode n =
  Node n [Node (n+1) [Node (n+2) []], Node (n+3) [Node (n+4) []]]

testDfs :: IO ()
testDfs = do
  let ns = map generateNode [1,6..60]
  let t = Tree $ Node 0 ns

  print $ treeWalkR t
  print $ treeWalkH t
  print $ treeWalkC t
  print $ treeWalkM t

--
