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

{-
treeWalkC' :: Node a -> [a]
treeWalkC' = loop id
  where
    loop :: ([a] -> [a]) -> Node a -> [a]
    loop cont (Node v cs) =
      let conts = foldr (\n comp r -> loop (comp . (r ++)) n)
                        cont
                        cs
      in conts [v]
-}

treeWalkC' :: Node a -> [a]
treeWalkC' n = loop n id
  where
    loop :: Node a -> ([a] -> [a]) -> [a]
    loop (Node v cs) cont =
      loopChildren cs $
        \res -> cont (v : res)

    loopChildren :: [Node a] -> ([a] -> [a]) -> [a]
    loopChildren [] cont = cont []
    loopChildren (c:cs) cont =
      loopChildren cs $
        \res -> cont (loop c id ++ res)


{-
data CPS r a = CPS { runCPS :: (a -> r) -> r }

instance Functor (CPS r) where
  -- fmap :: (a -> b) -> CPS r a -> CPS r b
  fmap f (CPS car) = CPS $ \cbr -> car (cbr . f)

instance Applicative (CPS r) where
  pure a = CPS $ \c -> c a
  -- (<*>) :: CPS r (a -> b) -> CPS r a -> CPS r b
  (CPS f) <*> (CPS car) =
    CPS $ \cbr -> f (\cab -> car (cbr . cab))

instance Monad (CPS r) where
  -- (>>=) :: CPS r a -> (a -> CPS r b) -> CPS r b
  (CPS car) >>= f =
    CPS $ \cbr -> car (\a -> runCPS (f a) cbr)


treeWalkC' :: Node a -> [a]
treeWalkC' n = runCPS (loop n) id
  where
    loop :: Node a -> CPS [a] [a]
    loop (Node v cs) = do
      rs <- mapM loop cs
      CPS $ \cont -> cont (v : concat rs)
-}

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
