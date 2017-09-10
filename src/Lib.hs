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
import Data.Maybe (maybe, isJust, catMaybes)
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
import Data.Aeson.Lens (key, _String)
import Safe (headMay)
import System.FilePath.Posix (takeFileName, (</>))

import Lib.Error (liftErr, NicoException(..))
import Lib.Operators ((<*>>))
import Lib.Url (websocketUri, host, port, path, appendPath)
import Lib.Cookie (renderCookieJar)
import Lib.FileSystem (prepareDirectory)
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
          >> keepReading c

        -- watchingInternval
-- {"type":"watch","body":{"room":{"messageServerUri":"ws://msg103.live.nicovideo.jp:83/websocket","messageServerType":"niwavided","roomName":"co2947819","threadId":"1611635152","forks":[0],"importedForks":[]},"command":"currentroom"}}
-- {type: "watch", body: {command: "watching", params: ["3874174796402", "-1", "0"]}}
        pong c = putStrLn "Sending pong..."
                 >> WS.sendTextData c ("{\"type\":\"pong\",\"body\":{}}" :: BC8.ByteString)

        keepReading c = WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received data: " <> str)
                        >> if maybe False ((==) "ping") (str ^? key "type" . _String)
                           then pong c
                           else return ()
                        >> keepReading c


        lookForM3U8Url :: B.ByteString -> Maybe T.Text
        lookForM3U8Url string = string ^? key "body"
                                        . key "currentStream"
                                        . key "uri"
                                        . _String

processMasterM3U8 :: String -> String -> IO ()
processMasterM3U8 base url =
    (go base url)
    `catch` (\e-> putStrLn (show (e :: HttpException))
                  >> processMasterM3U8 base url)
  where go base url = prepareDirectory base
                      >> get url
                      <&> view responseBody
                      <&> P.parsePlayListFromM3U8
                      <&> headMay
                      >>= liftErr NoPlayListFoundInMaster
                      <&> BC8.unpack
                      <&> appendPath url
                      >>= liftErr InvalidPlayListPath
                      >>= processPlayList base


processPlayList :: String -> String -> IO ()
processPlayList = go []
  where go existing base url = do
          putStrLn $ "fetching " <> url
          body <- view responseBody <$> get url
          let playList = catMaybes $ appendPath url <$> BC8.unpack
                                                    <$> P.parsePlayListFromM3U8 body
          let duration = P.parseDurationFromM3U8 body
          let newTS = filter (\u-> not $ elem (outputFilename base u) existing) playList
          let filenames = outputFilename base <$> newTS
          traverse (\u-> forkIO $ fetchAndSave (outputFilename base u) u) newTS
          threadDelay duration
          go (existing <> filenames) base url


fetchAndSave :: String -> String -> IO ()
fetchAndSave filename url = log >> go
  where log = putStrLn $ "fetching " <> (takeFileName url)
        go  = (get url
              <&> view responseBody
              -- >>= BC8.writeFile filename
              $> ()
              )
              `catch` (\e -> putStrLn $ show (e :: HttpException))

outputFilename :: String -> String -> String
outputFilename base url =
  base </> (takeWhile ((/=) '?') $ takeFileName url)
