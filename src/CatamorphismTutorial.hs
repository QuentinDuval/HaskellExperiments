module CatamorphismTutorial where


-- Without Catamorphisms

{-
data List a = Nil | Cons a (List a)

fromList :: [a] -> List a
fromList = foldr Cons Nil

instance Foldable List where
  foldr f acc Nil = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Show a => Show (List a) where
  show = show . foldr (:) []

-- Example of recursion

sumList' :: List Int -> Int
sumList' Nil = 0
sumList' (Cons x xs) = x + sumList' xs

-- Example of raw usage of foldr

sumList :: List Int -> Int
sumList = foldr (+) 0
-}

-- With Catamorphism

data ListR a r = Nil | Cons a r

instance Functor (ListR a) where
  fmap f Nil = Nil
  fmap f (Cons a r) = Cons a (f r)

-- type List a = ListR a (List a) -- Infinite recursion
newtype List a = List { unList :: ListR a (List a) }

nil :: List a
nil = List Nil

cons :: a -> List a -> List a
cons x xs = List (Cons x xs)

fromList :: [a] -> List a
fromList = foldr cons nil

cataList :: (ListR a b -> b) -> List a -> b
cataList f = f . fmap (cataList f) . unList

sumList :: List Int -> Int
sumList = cataList algebra where
  algebra Nil = 0
  algebra (Cons x prevSum) = x + prevSum

sumList' :: List Int -> Int
sumList' = ($ 0) . cataList algebra where
  algebra Nil = (+ 0)
  algebra (Cons x f) = (+ x) . f

--

--
