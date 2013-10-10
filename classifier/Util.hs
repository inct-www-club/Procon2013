module Util where

import Linear

data On b a = On b a

getOn :: On b a -> a
getOn (On _ a) = a

instance Eq b => Eq (On b a) where
    On x _ == On y _ = x == y

instance Ord b => Ord (On b a) where
    compare (On x _) (On y _) = compare x y

nubNear :: (Num a, Ord a) => a -> [V2 a] -> [V2 a]
nubNear r (v : vs) = v : nubNear r (filter ((>r^2) . qd v) vs)
nubNear r [] = []
