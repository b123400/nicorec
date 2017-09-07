module Error
    ( liftErr
    ) where

import Prelude
import Control.Monad.Catch (throwM, MonadThrow, Exception)
import Data.Maybe (Maybe(..))

liftErr :: MonadThrow m => Exception e => e -> Maybe a -> m a
liftErr _ (Just a) = return a
liftErr ex Nothing = throwM ex
