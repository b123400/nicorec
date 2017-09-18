module Lib.Utility where

import Data.List (elem, drop)
import Data.Monoid ((<>))

repeatDedup :: Monad m => Eq a => Int -> ((a -> Bool) -> m [a]) -> m [a]
repeatDedup limit action = go []
  where go existing = action ((flip elem) existing) >>= go . addElem existing
        addElem a b = drop (length a + length b - limit) (a <> b)
