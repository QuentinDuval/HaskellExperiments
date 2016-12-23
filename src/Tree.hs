module Tree where


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
    -- TODO: use Cont monad with mapM?
    loop cont (Node v []) = cont [v]
    loop cont (Node v (c:cs)) =
      let conts = foldl (\comp n ->
                          \r -> loop (comp . (r ++)) n)
                        (cont . (v :)) cs
      in loop conts c


--

testDfs :: IO ()
testDfs = do
  let ns = replicate 100 $ Node 1 [Node 2 [], Node 3 [Node 4 []]]
  let t = Tree $ Node 1 ns

  print $ treeWalkR t
  print $ treeWalkH t
  print $ treeWalkC t

--
