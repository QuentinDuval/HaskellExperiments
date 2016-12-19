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
    loop stack  visited (Node v (c:cs)) = loop (cs:stack) (v:visited) c
    loop []     visited (Node v []) = v : visited
    loop ([]:t) visited n = loop t visited n
    loop (h:t)  visited (Node v []) = loop (tail h:t) (v:visited) (head h)

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
                          \r' -> loop (comp . (r' ++)) n)
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
