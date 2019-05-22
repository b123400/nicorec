{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( login
    , extractWebSocketTokens
    , getM3U8Url
    , processMasterM3U8
    ) where

import Prelude
import Control.Monad (mfilter, join)
import Control.Monad.Loops (untilJust, whileJust_)
import Control.Lens ((^.), (^?), (.~), (?~), (<&>), view)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad.Catch (MonadCatch, MonadThrow, catchAll, throwM)
import Data.Functor (($>))
import Data.Function ((&))
import Data.List (any)
import Data.Maybe (isJust, catMaybes, listToMaybe, fromMaybe)
import Data.String.Utils (maybeRead)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import qualified Data.ByteString as BS -- ByteString Strict
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
import Network.HTTP.Client (CookieJar, HttpException, cookie_name)
import Network.HTTP.Client.Internal (expose)
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
import System.FilePath.Posix (takeFileName, (</>), (<.>))

import Conduit ( ConduitT
               , MonadIO
               , ResourceT
               , MonadResource
               , Void
               , (=$=)
               , await
               , liftIO
               , yieldMany
               , yield
               , runResourceT
               , runConduit )
import Data.Conduit.Async ((=$=&), runCConduit)
import Network.HTTP.Simple (parseRequest, httpSource, getResponseBody)
import Data.Conduit.Binary (sinkFile)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Lib.Error (liftErr, NicoException(..))
import Lib.Operators ((<*>>))
import Lib.Url (websocketUri, host, port, path, appendPath)
import Lib.Cookie (renderCookieJar)
import Lib.FileSystem (prepareDirectory)
import Lib.Utility (repeatDedup, retry)
import Lib.Conduit (dedup, fork, collect)
import qualified Lib.Parser as P

import Wuss

login :: String -> String -> IO (Maybe CookieJar)
login username password = do
  putStrLn "Logging in"
  r <- post "https://secure.nicovideo.jp/secure/login" [ "mail" := username
                                                       , "password" := password ]
  let isLoginSuccess = fromMaybe False $ any ((==) "user_session" . cookie_name)
                                      <$> expose
                                      <$> (r ^? responseCookieJar)

  putStrLn ("Login: " <> show isLoginSuccess)
  if isLoginSuccess
    then return $ r ^? responseCookieJar
    else return Nothing


fetchPlayerHTML :: CookieJar -> String -> IO B.ByteString
fetchPlayerHTML cookieJar liveID = do
  let opts = defaults & param "player_name" .~ ["leo"]
                      & cookies ?~ cookieJar
  r <- getWith opts $ "http://live2.nicovideo.jp/watch/" ++ liveID
  return $ r ^. responseBody

extractWebSocketTokens :: CookieJar -> String -> IO (Maybe P.WebsocketTokens)
extractWebSocketTokens cookieJar liveID =
  fetchPlayerHTML cookieJar liveID <&> P.extractWebSocketTokens


getM3U8Url :: P.WebsocketTokens -> CookieJar -> IO T.Text
getM3U8Url tokens jar = do
  uri <- liftErr CannotParseWebsocketUri $ websocketUri tokens
  result <- newEmptyMVar
  forkIO $ (join $ runSecureClientWith
            <$> host uri
            <*> pure 443
            <*>> path uri
            <*>> WS.defaultConnectionOptions
            <*>> [("Cookies", B.toStrict $ renderCookieJar jar)
                 ,("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36")
                 ]
            <*>> action result)
  takeMVar result

  where action :: MVar T.Text -> WS.Connection -> IO ()
        action result c =
          putStrLn "Websocket connection opened"
          >>  WS.sendTextData c ("{type: \"watch\", body: {command: \"playerversion\", params: [\"leo\"]}}" :: BC8.ByteString)
          >>  WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"command\":\"getpermit\",\"requirement\":{\"broadcastId\":\""
                               <> P.broadcastId tokens
                               <> "\",\"route\":\"\",\"stream\":{\"protocol\":\"hls\",\"requireNewStream\":true,\"priorStreamQuality\":\"high\",\"isLowLatency\":true},\"room\":{\"isCommentable\":true,\"protocol\":\"webSocket\"}}}}")
          >>  untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForM3U8Url str)
          >>= putMVar result
          >>  untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForWatchingInterval str)
          >>= forkIO . (watchingInterval c (P.broadcastId tokens))
          >>  keepReading c
          >>  putStrLn "Websocket closed"

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
                        >> (if isJust $ mfilter ((==) "disconnect") (str ^? key "body" . key "command" . _String)
                            then return ()
                            else keepReading c)

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
                                       &   mfilter ((==) "watchinginterval")
                                       >>  string ^? key "body"
                                                   . key "params"
                                                   . nth 0
                                                   . _String
                                       <&> T.unpack

processMasterM3U8 :: String -> String -> IO ()
processMasterM3U8 base url =  prepareDirectory base
                              >>  get url
                              <&> view responseBody
                              <&> P.parsePlayListFromM3U8
                              <&> listToMaybe
                              >>= liftErr NoPlayListFoundInMaster
                              <&> BC8.unpack
                              <&> appendPath url
                              >>= liftErr InvalidPlayListPath
                              >>= processPlayList base


processPlayList :: String -> String -> IO ()
processPlayList base url =
  round <$> getPOSIXTime >>= \timeStamp->
  runResourceT $ runCConduit
               $ source =$=& (dedup 100)
                        =$=& fetch
                        =$=& fork
                        =$=& collect
                        =$=& (sink $ show timeStamp <.> "ts")

  where source :: MonadIO m => ConduitT () String m ()
        source = (liftIO $ putStrLn $ "fetching " <> url)
             >>  (liftIO $ view responseBody <$> (retry 20 $ get url))
             >>= \body ->
             let playList = catMaybes $ appendPath url <$> BC8.unpack
                                                       <$> P.parsePlayListFromM3U8 body
                 duration = P.parseDurationFromM3U8 body
             in  yieldMany playList
             -- Try to reduce missing frame by making interval lower
             >>  (liftIO $ threadDelay $ round $ (fromIntegral duration) * 0.8)
             -- need to figure out when to end
             >>  source

        fetch :: MonadIO m => ConduitT String (ConduitT () BS.ByteString (ResourceT IO) ()) m ()
        fetch = whileJust_ await $ \url->
                  (liftIO $ parseRequest url)
                  >>= \req-> (yield $ httpSource req getResponseBody)
                  >>  (liftIO $ putStrLn $ "fetching " <> url)

        sink :: MonadResource m => String -> ConduitT BS.ByteString Void m ()
        sink filename = sinkFile $ base </> filename
