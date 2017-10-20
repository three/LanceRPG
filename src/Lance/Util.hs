module Lance.Util (
    ifte,
    until
    ) where

import Protolude

ifte :: a -> a -> Bool -> a
ifte x y b = case b of
    True  -> x
    False -> y

until :: Monad m => m Bool -> m ()
until m = m >>= ifte pass (until m)
