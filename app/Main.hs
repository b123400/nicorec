module Main where

import Control.Lens ((<&>))
import Data.Functor (($>))

import System.ReadEnvVar (lookupEnvEx)
import qualified Data.ByteString.Lazy.Char8 as B

import Lib (login, extractWebSocketTokens, getM3U8Url, NicoException(..))
import Error (liftErr)

main :: IO ()
main = do
  username <- lookupEnvEx "NICONICO_USERNAME"
  password <- lookupEnvEx "NICONICO_PASSWORD"
  liveID <- lookupEnvEx "LIVE_ID"

  cookie <- login username password >>= (liftErr CannotLogin)
  tokens <- extractWebSocketTokens cookie liveID >>= (liftErr CannotExtractToken)
  text <- getM3U8Url tokens cookie
  putStrLn $ show $ text
