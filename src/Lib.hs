{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( login
    , extractWebSocketTokens
    , getM3U8Url
    , processMasterM3U8
    ) where

import Prelude
import Control.Monad (mfilter, join)
import Control.Monad.Loops (untilJust)
import Control.Lens ((^.), (^?), (.~), (?~), (<&>), view)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad.Catch (catch)
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Data.String.Utils (maybeRead)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
import Network.HTTP.Client (CookieJar, HttpException)
import Network.Wreq ( get
                    , post
                    , getWith
                    , defaults
                    , param
                    , cookies
                    , responseHeader
                    , responseHeaders
                    , responseBody
                    , responseCookieJar
                    , FormParam((:=)))
import qualified Network.WebSockets as WS
import Data.Aeson.Lens (key, nth, _String, _Number)
import System.FilePath.Posix (takeFileName, (</>))

import Lib.Error (liftErr, NicoException(..))
import Lib.Operators ((<*>>))
import Lib.Url (websocketUri, host, port, path, appendPath)
import Lib.Cookie (renderCookieJar)
import Lib.FileSystem (prepareDirectory)
import Lib.Utility (repeatDedup)
import qualified Lib.Parser as P

login :: String -> String -> IO (Maybe CookieJar)
login username password = do
  putStrLn "Logging in"
  r <- post "https://secure.nicovideo.jp/secure/login" [ "mail" := username
                                                       , "password" := password ]
  let isLoginSuccess =
        r ^? responseHeader "x-niconico-authflag"
        & mfilter ((==) "1")
        & isJust

  putStrLn ("Login: " <> show isLoginSuccess)
  if isLoginSuccess
    then return $ r ^? responseCookieJar
    else return Nothing


fetchPlayerHTML :: CookieJar -> String -> IO B.ByteString
fetchPlayerHTML cookieJar liveID = do
  let opts = defaults & param "player_name" .~ ["leo"]
                      & cookies ?~ cookieJar
  r <- getWith opts $ "http://live2.nicovideo.jp/select_player/" ++ liveID
  return $ r ^. responseBody

extractWebSocketTokens :: CookieJar -> String -> IO (Maybe P.WebsocketTokens)
extractWebSocketTokens cookieJar liveID =
  fetchPlayerHTML cookieJar liveID <&> P.extractWebSocketTokens


getM3U8Url :: P.WebsocketTokens -> CookieJar -> IO T.Text
getM3U8Url tokens jar = do
  uri <- liftErr CannotParseWebsocketUri $ websocketUri tokens
  result <- newEmptyMVar
  forkIO $ (join $ WS.runClientWith
            <$> host uri
            <*> port uri
            <*>> path uri
            <*>> WS.defaultConnectionOptions
            <*>> [("Cookies", B.toStrict $ renderCookieJar jar)]
            <*>> action result)
  takeMVar result

  where action :: MVar T.Text -> WS.Connection -> IO ()
        action result c =
          putStrLn "Websocket connection opened"
          >> WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"params\":[\""
                             <> P.broadcastId tokens
                             <> "\",\"\",\"true\",\"hls\",\"\"],\"command\":\"getpermit\"}}")
          >> untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForM3U8Url str)
          >>= putMVar result
          >> untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForWatchingInterval str)
          >>= forkIO . (watchingInterval c (P.broadcastId tokens))
          >> keepReading c
          >> putStrLn "Websocket closed"

        watchingInterval c broadcastId interval =
          putStrLn "Sending watching command"
          >> WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"command\":\"watching\",\"params\":[\""
                                <> broadcastId
                                <> "\",\"-1\",\"0\"]}}")
          >> threadDelay interval
          >> watchingInterval c broadcastId interval

        pong c = putStrLn "Sending pong..."
                 >> WS.sendTextData c ("{\"type\":\"pong\",\"body\":{}}" :: BC8.ByteString)

        keepReading c = WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received data: " <> str)
                        >> (if isJust $ mfilter ((==) "ping") (str ^? key "type" . _String)
                           then pong c
                           else return ())
                        >> keepReading c

        lookForM3U8Url :: B.ByteString -> Maybe T.Text
        lookForM3U8Url string = string ^? key "body"
                                        . key "currentStream"
                                        . key "uri"
                                        . _String

        -- Niconico requires client side to report "I am watching" for every interval
        -- We can get the interval from websocket,
        -- The report will be handled in `watchingInterval`
        lookForWatchingInterval :: B.ByteString -> Maybe Int
        lookForWatchingInterval string =
          ((*) 1000000) <$> (maybeRead =<< findInterval string)
          where findInterval string  = (string ^? key "body" . key "command")
                                       & mfilter ((==) "watchinginterval")
                                       >> string ^? key "body" . key "params" . nth 0 . _String
                                       <&> T.unpack

processMasterM3U8 :: String -> String -> IO ()
processMasterM3U8 base url =  prepareDirectory base
                              >> get url
                              <&> view responseBody
                              <&> P.parsePlayListFromM3U8
                              <&> listToMaybe
                              >>= liftErr NoPlayListFoundInMaster
                              <&> BC8.unpack
                              <&> appendPath url
                              >>= liftErr InvalidPlayListPath
                              >>= processPlayList base


processPlayList :: String -> String -> IO ()
processPlayList base url = repeatDedup 100 go $> ()
  where go performed = do
          putStrLn $ "fetching " <> url
          body <- view responseBody <$> get url
          let playList = catMaybes $ appendPath url <$> BC8.unpack
                                                    <$> P.parsePlayListFromM3U8 body
          let duration = P.parseDurationFromM3U8 body
          let newTS = filter (not . performed . outputFilename base) playList
          let filenames = outputFilename base <$> newTS
          traverse (\u-> forkIO $ fetchAndSave (outputFilename base u) u) newTS
          threadDelay (round $ (fromIntegral duration) * 0.8) -- Try to reduce missing frame by making interval lower
          return filenames


fetchAndSave :: String -> String -> IO ()
fetchAndSave filename url = log >> go 10
  where log = putStrLn $ "fetching " <> (takeFileName url)
        go 0          = return ()
        go retryCount = (putStrLn ("retry: " <> (show retryCount))
                        >> get url
                        <&> view responseBody
                        >>= BC8.writeFile filename)
                        `catch` (\e -> (putStrLn $ show (e :: HttpException)) >> go (retryCount - 1))

outputFilename :: String -> String -> String
outputFilename base url =
  base </> (takeWhile ((/=) '?') $ takeFileName url)
