module Lib.Operators where

import Control.Applicative (Applicative, (<*>), pure)
import Data.Monoid (mempty)
import Data.Maybe (maybe)

infixl 4 <*>>

(<*>>) :: Applicative f => f (a -> b) -> a -> f b
(<*>>) m a = m <*> (pure a)

infixl 4 >>=?

(>>=?) :: Monad m => Monoid b => m (Maybe a) -> (a -> m b) -> m b
(>>=?) m whenJust = m >>= maybe (return mempty) whenJust
