module Util where

import Linear

data On b a = On b a

getOn :: On b a -> a
getOn (On _ a) = a

instance Eq b => Eq (On b a) where
    On x _ == On y _ = x == y

instance Ord b => Ord (On b a) where
    compare (On x _) (On y _) = compare x y
