module Main where

import System.ReadEnvVar (lookupEnvDef, lookupEnvEx)
import Control.Monad (join, forever, forM_)
import Control.Monad.Loops (untilJust)
import Control.Concurrent (forkIO, threadDelay)
import Data.Functor (($>))
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Time.Clock (getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import System.FilePath.Posix ((</>))

import System.IO (hFlush, stdout)

import Lib (login, extractWebSocketTokens, record)
import Lib.Error (liftErr, NicoException(..))
import Lib.Community (getLiveID)
import Lib.Utility (retry', neverGiveUp, neverGiveUp')
import qualified Lib.Channel as C (getLiveID)

main :: IO ()
main = do
  username <- lookupEnvEx "NICONICO_USERNAME"
  password <- lookupEnvEx "NICONICO_PASSWORD"
  base <- lookupEnvDef "OUT_DIR" ""
  coIds <- lookupEnvEx "CO_ID"

  cookieJar <- liftErr CannotLogin =<< login username password

  forM_ (splitOn "," coIds) $ \coId-> forkIO $ neverGiveUp $ do
    liveID <- neverGiveUp' (getCurrentTime >>= \currentTime-> putStrLn ("No live(" ++ coId ++ ")" ++ (show currentTime))
                         >> delay1Minute
                         >> putStrLn "Retrying")
                         (if ("co" `isPrefixOf` coId)
                              then (getLiveID cookieJar coId)
                              else (C.getLiveID cookieJar coId))

    tokens <- liftErr CannotExtractToken =<< (retry' 10 delay1Minute $ extractWebSocketTokens cookieJar liveID)
    record tokens cookieJar (base </> coId)
    putStrLn "Done"

  forever $ do
    threadDelay (1000 * 1000)
    hFlush stdout


delay1Minute :: IO ()
delay1Minute = threadDelay (1 * 60 * 1000 * 1000)
