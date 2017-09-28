{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module DepTypes () where


class Func a where
  type Res a :: *

instance Func Int where
  type Res Int = Int

instance Func String where
  type Res String = String

-- Does not compile ! Cannot deduce a
-- f :: Res a -> ()
-- f _ = ()

class FuncInj a where
  data ResInj a :: *

instance FuncInj Int where
  data ResInj Int = FuncInt

instance FuncInj String where
  data ResInj String = FuncString

g :: ResInj a -> ()
g _ = ()

--
