{-# LANGUAGE GADTs, TypeFamilies #-}
module BulkSave where


-- Aliases

type DB = IO


--------------------------------------------------------------------------------
-- A la OOP style
-- * Reasonning by instances
-- * At least you can get "a la carte"
-- * Even constraints in Java <T extends ISavable> forces to do in the instance
-- * Concepts could help a lot?
-- * Main problem is that we limit ourselves to single instances (limited optims)
--------------------------------------------------------------------------------

class ISaveObject a where
  type ObjectId a :: *
  save :: a -> DB ()
  load :: ObjectId a -> DB a

-- Type Erasure
data SavableObject where
  MkSavableObject :: (ISaveObject a) => a -> SavableObject

instance ISaveObject SavableObject where
  type ObjectId SavableObject = Double -- TODO: basically forces everyone to have this
  save (MkSavableObject a) = save a
  load identifier = load identifier

-- Callable for many objects, or type erased objects
saveAll' :: (Traversable t, ISaveObject a) => t a -> DB ()
saveAll' xs = traverse save xs >> pure ()


--------------------------------------------------------------------------------
-- A la Haskell style
-- * Type erasure does not make much sense in this case
-- * Works for all traversable (no need to spec for list, maps, etc.)
-- * In Clojure, you would be saved by the fact that collection implement `seq`
--   and the fact that you have dynamic typing...
--   YET, you cannot express this easily
--------------------------------------------------------------------------------

class ISaveCollection a where
  type SearchCriteria a :: *
  saveAll :: (Traversable t) => t a -> DB ()
  loadAll :: SearchCriteria a -> DB [a] -- TODO: Foldable would be usefless?

data B = B { bId :: Double
           , bData :: String }
  deriving (Show, Eq, Ord)

instance ISaveCollection B where
  type SearchCriteria B = Double
  saveAll = undefined
  loadAll c = pure [B 1 "toto"]


--------------------------------------------------------------------------------
-- Conclusion
-- * A la carte in both cases (not in OOP)
-- * Oriented toward instances in Clojure (and type erasure)
-- * Oriented toward constraints in Haskell (and no type erasure)
--------------------------------------------------------------------------------




--
