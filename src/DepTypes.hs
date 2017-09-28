{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module DepTypes () where


class Func a where
  type Res a :: *

instance Func Int where
  type Res Int = Int

instance Func String where
  type Res String = String

-- TODO: how to test the failure of deduction?

class FuncInj a where
  data ResInj a :: *

instance FuncInj Int where
  data ResInj Int = FuncInt

instance FuncInj String where
  data ResInj String = FuncString

--
