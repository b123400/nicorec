module Main where

import System.ReadEnvVar (lookupEnvEx)
import Control.Monad (join)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

import Lib (login, extractWebSocketTokens, getM3U8Url, processMasterM3U8)
import Lib.Error (liftErr, NicoException(..))
import Lib.Community (getLiveID)

main :: IO ()
main = do
  username  <- lookupEnvEx "NICONICO_USERNAME"
  password  <- lookupEnvEx "NICONICO_PASSWORD"
  liveID    <- lookupEnvEx "LIVE_ID"
  baseOutputDir <- lookupEnvEx "OUT_DIR"

  -- coId      <- lookupEnvEx "CO_ID"

  cookieJar <- login username password >>= liftErr CannotLogin
  -- liveID    <- getLiveID cookieJar coId
  -- putStrLn $ show liveID

  tokens    <- extractWebSocketTokens cookieJar liveID >>= liftErr CannotExtractToken
  m3u8Url   <- getM3U8Url tokens cookieJar

  processMasterM3U8 baseOutputDir $ T.unpack m3u8Url
  putStrLn "Done"
