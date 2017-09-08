module Lib.Operators where

import Control.Applicative (Applicative, (<*>), pure)

infixl 4 <*>>

(<*>>) :: Applicative f => f (a -> b) -> a -> f b
(<*>>) m a = m <*> (pure a)
