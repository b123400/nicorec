module Lib.Utility where

import Data.List (elem, drop)
import Data.Monoid ((<>))
import Control.Monad.Catch (MonadCatch, MonadThrow, catchAll)
import Control.Monad.IO.Class (MonadIO, liftIO)

repeatDedup :: Monad m => Eq a => Int -> ((a -> Bool) -> m [a]) -> m [a]
repeatDedup limit action = go []
  where go existing = action ((flip elem) existing) >>= go . addElem existing
        addElem a b = drop (length a + length b - limit) (a <> b)

retry :: MonadThrow m => MonadCatch m => Int -> m a -> m a
retry c = retry' c (return ())

retry' :: MonadThrow m => MonadCatch m => Int -> m b -> m a -> m a
retry' 0 _ io = io
retry' count beforeRetry io = catchAll io (const $ beforeRetry >> retry (count - 1) io)

neverGiveUp :: MonadThrow m => MonadCatch m => MonadIO m => m a -> m a
neverGiveUp = neverGiveUp' (return ())

neverGiveUp' :: MonadThrow m => MonadCatch m => MonadIO m => m b -> m a -> m a
neverGiveUp' beforeRetry m = catchAll m (\e -> (liftIO $ putStrLn $ show e) >> (neverGiveUp $ beforeRetry >> m))
