module Lib.Error where

import Prelude
import Control.Monad.Catch (throwM, MonadThrow, Exception)
import Data.Maybe (Maybe(..))
import Data.Typeable (Typeable)

data NicoException = CannotLogin
                   | CannotExtractToken
                   | CannotParseWebsocketUri
                   | NoPlayListFoundInMaster
                   | InvalidPlayListPath
                   | NoLiveInCommunity
  deriving (Show, Typeable)

instance Exception NicoException

liftErr :: MonadThrow m => Exception e => e -> Maybe a -> m a
liftErr _ (Just a) = return a
liftErr ex Nothing = throwM ex
