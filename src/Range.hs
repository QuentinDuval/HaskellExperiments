module Range where

-- Open range on the right [low, high)
data Range = Range { low :: !Int, high :: !Int }
