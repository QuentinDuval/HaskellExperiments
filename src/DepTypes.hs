{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module DepTypes () where


-- Non injective type function

class Func a where
  type Res a :: *

instance Func Int where
  type Res Int = Int

instance Func String where
  type Res String = String

-- Does not compile ! Cannot deduce a
-- f :: Res a -> IO ()
-- f _ = print "Does not compile (type `a` is ambiguous)"

-- Providing hints to the compiler

data Proxy a = Proxy

f :: Proxy a -> Res a -> IO ()
f _ _ = print "Useless function, but compiles fine"


-- Injective type function

class FuncInj a where
  data ResInj a :: *

instance FuncInj Int where
  data ResInj Int = FuncInt

instance FuncInj String where
  data ResInj String = FuncString

g :: ResInj a -> ()
g _ = ()

--
