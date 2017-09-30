module Main where

import System.ReadEnvVar (lookupEnvDef, lookupEnvEx)
import Control.Monad (join, forever)
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
import Lib.Utility (retry', neverGiveUp')

main :: IO ()
main =
  lookupEnvEx "NICONICO_USERNAME" >>= \username ->
  lookupEnvEx "NICONICO_PASSWORD" >>= \password ->
  -- lookupEnvEx "LIVE_ID"        >>= \liveID ->
  lookupEnvDef "OUT_DIR" ""       >>= \base ->
  lookupEnvEx "CO_ID"             >>= \coId ->

  login username password
          >>= liftErr CannotLogin >>= \cookieJar ->

  forever $

  neverGiveUp' (putStrLn "No live" -- also need to print time
                >> delay1Minute
                >> putStrLn "Retrying")
               (getLiveID cookieJar coId)
                                  >>= \liveID ->
  -- putStrLn $ show liveID

  -- TODO: handle non-premium cannot watch
  (retry' 10 delay1Minute $ extractWebSocketTokens cookieJar liveID)
   >>= liftErr CannotExtractToken >>= \tokens->

  getM3U8Url tokens cookieJar     >>= \m3u8Url ->

  (processMasterM3U8 (base </> coId) $ T.unpack m3u8Url)
  >> putStrLn "Done"


delay1Minute :: IO ()
delay1Minute = threadDelay (1 * 60 * 1000 * 1000)
