{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
-- Mixing types and typeclasses in overloads.

class Greetable g where
  hi :: g -> String

-- Here i should list all instances of Num
instance {-# OVERLAPS #-} (Show n, Num n) => Greetable n where
  hi s = "Hi number! " ++ show s


-- Here is other stuff
instance {-# OVERLAPPABLE #-} Greetable String where
  hi s = "Hi string! " ++ s
