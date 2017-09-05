module Main where

import Control.Lens ((<&>))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (throwM, MonadThrow, Exception)
import System.ReadEnvVar (lookupEnvEx)
import Data.Typeable (Typeable)

import Lib (login, extractWebSocketTokens)

data NicoException = CannotLogin | CannotExtractToken
  deriving (Show, Typeable)

instance Exception NicoException

main :: IO ()
main = do
  username <- lookupEnvEx "NICONICO_USERNAME"
  password <- lookupEnvEx "NICONICO_PASSWORD"
  liveID <- lookupEnvEx "LIVE_ID"

  cookie <- login username password >>= (liftErr CannotLogin)
  tokens <- extractWebSocketTokens cookie liveID >>= (liftErr CannotExtractToken)
  putStrLn $ show tokens


liftErr :: MonadThrow m => Exception e => e -> Maybe a -> m a
liftErr _ (Just a) = return a
liftErr ex Nothing = throwM ex
