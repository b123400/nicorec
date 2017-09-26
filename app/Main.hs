module Main where

import System.ReadEnvVar (lookupEnvDef, lookupEnvEx)
import Control.Monad (join)
import Control.Monad.Loops (untilJust)
import Control.Monad.Catch (catch, throwM)
import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import System.FilePath.Posix ((</>))

import Lib (login, extractWebSocketTokens, getM3U8Url, processMasterM3U8)
import Lib.Error (liftErr, NicoException(..))
import Lib.Community (getLiveID)

main :: IO ()
main = do
  username  <- lookupEnvEx "NICONICO_USERNAME"
  password  <- lookupEnvEx "NICONICO_PASSWORD"
  -- liveID    <- lookupEnvEx "LIVE_ID"
  base      <- lookupEnvDef "OUT_DIR" ""
  coId      <- lookupEnvEx "CO_ID"

  cookieJar <- login username password >>= liftErr CannotLogin

  liveID    <- untilJust ((Just <$> getLiveID cookieJar coId)
                          `catch` (\e-> case e of
                                     NoLiveInCommunity ->
                                       putStrLn "No live"
                                       >> threadDelay (5 * 60 * 1000 * 1000)
                                       >> putStrLn "Retrying"
                                       $> Nothing
                                     _ -> throwM e))
  -- putStrLn $ show liveID

  -- TODO: handle non-premium cannot watch
  tokens    <- extractWebSocketTokens cookieJar liveID >>= liftErr CannotExtractToken
  m3u8Url   <- getM3U8Url tokens cookieJar

  processMasterM3U8 (base </> coId) $ T.unpack m3u8Url
  putStrLn "Done"
